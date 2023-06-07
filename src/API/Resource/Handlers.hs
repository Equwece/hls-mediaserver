{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module API.Resource.Handlers (resultServer, authHandler, genAuthServerContext) where

import API.APISpec (RestAPI)
import API.External.Postgres (PostgresClass (addRefreshQuery, deleteRefreshByIdQuery, deleteRefreshByUserIdQuery, getRefreshByIdQuery, getResourceByIdQuery, getUserByIdQuery, getUserByUsernameQuery, listResourcesQuery))
import API.External.Segmentor (segmentContent)
import API.Interfaces (AppEnvironment (AppEnvironment, db, jwtEncodeSecret, logger), Logger (logMsg))
import API.Models (Account (Account), AuthInput (AuthInput, password, username), JwtTokens (JwtTokens), RawHtml (RawHtml), RefreshInput (RefreshInput, refresh))
import API.Resource.Models (Resource)
import API.Users.Handlers (userServer)
import qualified API.Users.Models as UM
import Control.Lens ((&), (.~), (?~))
import Control.Monad.Cont (MonadIO (liftIO), unless, when)
import qualified Data.ByteString as B
import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.OpenApi (HasInfo (info), HasLicense (license), HasTitle (title), HasVersion (version), OpenApi)
import Data.Password.Bcrypt (PasswordCheck (PasswordCheckFail, PasswordCheckSuccess), PasswordHash (PasswordHash, unPasswordHash), checkPassword, hashPassword, mkPassword)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (NominalDiffTime, nominalDay)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID (UUID, fromText, toText)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Network.HTTP.Types.Header (hAuthorization)
import Network.Wai (Request (requestHeaders))
import Servant (Application, Context (EmptyContext), Handler, NoContent (NoContent), Proxy (Proxy), Server, ServerError (errReasonPhrase), ServerT, Tagged, err401, err403, err404, serveDirectoryWebApp, throwError, type (:<|>) (..))
import Servant.API (Raw)
import Servant.OpenApi (HasOpenApi (toOpenApi))
import Servant.Server
  ( Application,
    Context (EmptyContext, (:.)),
    Handler,
    HasServer (ServerT),
    Server,
    ServerError (errReasonPhrase),
    Tagged,
    err401,
    err403,
    err404,
  )
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import Web.JWT (Algorithm (HS256), EncodeSigner (EncodeHMACSecret), JOSEHeader (JOSEHeader, alg, cty, kid, typ), JWTClaimsSet (JWTClaimsSet, iss, sub), encodeSigned, numericDate, stringOrURI)
import qualified Web.JWT as JWT

resultServer appEnv =
  -- clientAppServer appEnv :<|> staticServer :<|> (restApiServer appEnv :<|> swaggerServer)
  clientAppServer appEnv :<|> staticServer :<|> (restApiServer appEnv)

restApiServer appEnv =
  ( resourceServer appEnv
      :<|> userServer appEnv
  )
    :<|> (issueToken appEnv :<|> refreshToken appEnv)

issueToken :: AppEnvironment -> AuthInput -> Handler JwtTokens
issueToken appEnv@(AppEnvironment {..}) (AuthInput {..}) = do
  authUser <- liftIO $ getUserByUsernameQuery db username
  if not (null authUser)
    then do
      let passHash = PasswordHash $ T.pack . UM.password $ head authUser
          pass = mkPassword (T.pack password)
          passCheck = checkPassword pass passHash
      case passCheck of
        PasswordCheckSuccess -> do
          liftIO $ logMsg logger $ "Create tokens for " <> username
          _ <- liftIO $ deleteRefreshByUserIdQuery db (UM.id $ head authUser)
          createTokenPair appEnv (head authUser)
        PasswordCheckFail -> do
          liftIO $ logMsg logger $ "Wrong auth creds for " <> username
          throwError err401
    else throwError err404

createTokenPair :: AppEnvironment -> UM.User -> Handler JwtTokens
createTokenPair AppEnvironment {..} authUser = do
  newRefreshId <- head <$> liftIO (addRefreshQuery db (UM.id authUser))
  currentNumericDate <- liftIO getPOSIXTime
  let signer = JWT.hmacSecret jwtEncodeSecret
      joseHeaders =
        mempty
          { typ = Just "JWT",
            alg = Just HS256
          }
      jwtClaimsAccess =
        mempty
          { iss = stringOrURI . T.pack $ "HLS Mediaserver API",
            sub = stringOrURI . toText $ UM.id authUser,
            JWT.exp = numericDate nominalDay,
            JWT.iat = numericDate currentNumericDate
          }
      jwtClaimsRefresh =
        mempty
          { iss = stringOrURI . T.pack $ "HLS Mediaserver API",
            sub = stringOrURI . toText $ newRefreshId,
            JWT.exp = numericDate (nominalDay * 30),
            JWT.iat = numericDate currentNumericDate
          }
      accessToken = T.unpack $ encodeSigned signer joseHeaders jwtClaimsAccess
      refreshToken = T.unpack $ encodeSigned signer joseHeaders jwtClaimsRefresh
  return (JwtTokens accessToken refreshToken)

refreshToken :: AppEnvironment -> RefreshInput -> Handler JwtTokens
refreshToken appEnv@(AppEnvironment {..}) (RefreshInput {..}) = do
  let verifier = JWT.toVerify . JWT.hmacSecret $ jwtEncodeSecret
      mJWT = JWT.decodeAndVerifySignature verifier (T.pack refresh)
  case mJWT of
    Just validJWT -> do
      currentNumericDate <- liftIO getPOSIXTime
      let jwtClaims = JWT.claims validJWT
          mJwtCreationTime = JWT.iat jwtClaims
          mJwtExpTime = JWT.exp jwtClaims
          expCheckResult = do
            tokenIat <- mJwtCreationTime
            tokenExp <- mJwtExpTime
            let checkResult =
                  checkTokenTime
                    (JWT.secondsSinceEpoch tokenIat)
                    (JWT.secondsSinceEpoch tokenExp)
                    currentNumericDate
            return checkResult
      case expCheckResult of
        Just True -> do
          let mJwtSub = JWT.sub jwtClaims >>= fromText . JWT.stringOrURIToText
          case mJwtSub of
            Nothing -> do
              liftIO $ logMsg logger "Refresh token has not its uuid"
              throwError (err403 {errReasonPhrase = "Refresh token has not its uuid"})
            Just tokenId -> do
              mTokenUserId <- liftIO (getRefreshByIdQuery db tokenId)
              when (null mTokenUserId) (throwError err404)
              let (_, tokenUserId) = head mTokenUserId
              _ <- liftIO $ deleteRefreshByIdQuery db tokenId
              mUser <- liftIO $ getUserByIdQuery db tokenUserId
              when (null mUser) (throwError err404)
              liftIO . logMsg logger $ "Refresh tokens for " <> UM.username (head mUser)
              createTokenPair appEnv (head mUser)
        _ -> do
          liftIO $ logMsg logger "Token is expired"
          throwError (err403 {errReasonPhrase = "Token is expired"})
    Nothing -> do
      liftIO $ logMsg logger "Not valid refresh token"
      throwError (err403 {errReasonPhrase = "Not valid refresh token"})

extractTokenFromHeader :: AppEnvironment -> Request -> Handler (Maybe String)
extractTokenFromHeader appEnv@(AppEnvironment {..}) req = do
  let headersMap = Map.fromList $ requestHeaders req
      authHeader = do
        headerByteStr <- Map.lookup hAuthorization headersMap
        let headerContent = T.unpack . T.decodeUtf8 $ headerByteStr
        parseAuthHeader headerContent
  return authHeader
  where
    parseAuthHeader :: String -> Maybe String
    parseAuthHeader headerContent = maybeToken
      where
        headerList = words headerContent
        maybeToken =
          if length headerList /= 2
            then Nothing
            else Just $ headerList !! 1

validateToken :: AppEnvironment -> String -> Handler (Maybe UUID)
validateToken appEnv@(AppEnvironment {..}) token = do
  let verifier = JWT.toVerify . JWT.hmacSecret $ jwtEncodeSecret
      mJWT = JWT.decodeAndVerifySignature verifier (T.pack token)
  case mJWT of
    Just validJWT -> do
      let jwtClaims = JWT.claims validJWT
      checkResult <- checkTokenIsNotExp validJWT jwtClaims
      if checkResult
        then do
          let mJwtSub = JWT.sub jwtClaims >>= fromText . JWT.stringOrURIToText
          return mJwtSub
        else do
          liftIO $ logMsg logger "Token is expired"
          throwError (err403 {errReasonPhrase = "Token is expired"})
    Nothing -> do
      liftIO $ logMsg logger "Not valid token"
      throwError (err403 {errReasonPhrase = "Not valid token"})

checkTokenIsNotExp :: MonadIO m => p -> JWTClaimsSet -> m Bool
checkTokenIsNotExp validJWT jwtClaims = do
  currentNumericDate <- liftIO getPOSIXTime
  let mJwtCreationTime = JWT.iat jwtClaims
      mJwtExpTime = JWT.exp jwtClaims
      expCheckResult = do
        tokenIat <- mJwtCreationTime
        tokenExp <- mJwtExpTime
        let checkResult =
              checkTokenTime
                (JWT.secondsSinceEpoch tokenIat)
                (JWT.secondsSinceEpoch tokenExp)
                currentNumericDate
        return checkResult
  case expCheckResult of
    Just True -> return True
    _ -> return False

checkTokenTime :: NominalDiffTime -> NominalDiffTime -> NominalDiffTime -> Bool
checkTokenTime tokenIat tokenExp currentTime
  | currentTime <= (tokenIat + tokenExp) = True
  | otherwise = False

authHandler :: AppEnvironment -> AuthHandler Request Account
authHandler appEnv@AppEnvironment {..} = mkAuthHandler handler
  where
    -- handler req = maybe (throwError err401) (validateToken appEnv) (extractTokenFromHeader appEnv req)
    handler req = do
      mToken <- extractTokenFromHeader appEnv req
      case mToken of
        Just token -> do
          mUserId <- validateToken appEnv token
          case mUserId of
            Just uId -> return $ Account uId
            Nothing -> throwError err401
        Nothing -> throwError err401

genAuthServerContext :: AppEnvironment -> Context (AuthHandler Request Account ': '[])
genAuthServerContext appEnv = (authHandler appEnv) :. EmptyContext

-- swaggerServer :: Server (SwaggerSchemaUI api b)
-- swaggerServer = swaggerSchemaUIServer openApiSpec

clientAppServer :: AppEnvironment -> [Text] -> Handler RawHtml
clientAppServer appEnv@(AppEnvironment {..}) appPath = do
  appHtml <- liftIO $ readFile "./templates/index.html"
  return . RawHtml $ fromString appHtml

resourceServer appEnv@(AppEnvironment {..}) acc =
  updateResources
    :<|> (listResources :<|> resourceEntityServer appEnv)
  where
    listResources :: Handler [Resource]
    listResources = do
      resources <- liftIO $ listResourcesQuery db
      liftIO $ logMsg logger "List Resources"
      return resources
    updateResources :: Handler NoContent
    updateResources = do
      liftIO $ segmentContent appEnv
      liftIO $ logMsg logger "Library was updated"
      return NoContent

resourceEntityServer (AppEnvironment {..}) resId =
  getResource resId :<|> staticSegmentServer resId :<|> staticSegmentServer resId
  where
    getResource :: UUID -> Handler Resource
    getResource rId = do
      res <- liftIO $ getResourceByIdQuery db rId
      liftIO $ logMsg logger ("Get Resource " <> show rId)
      if not (null res)
        then return (head res)
        else throwError err404

staticSegmentServer :: UUID -> ServerT Raw Handler
staticSegmentServer _ = serveDirectoryWebApp "./data/segments/"

staticServer :: ServerT Raw Handler
staticServer = serveDirectoryWebApp "./static/"

-- openApiSpec :: OpenApi
-- openApiSpec =
--   toOpenApi (Proxy :: Proxy RestAPI)
--     & info . title .~ "Mediaserver API"
--     & info . version .~ "1.0"
--     & info . license ?~ "LGPL"

module JiraClient.ApiRequest where

import qualified Data.ByteString.Base64 as Base64
import Data.Kind (Type)
import JiraClient.Types
import Network.HTTP.Simple
import Qtility

class JiraRequest request where
  type ResponseType request :: Type
  makeRequest :: Credentials -> BaseUrl -> request -> IO (Either String (ResponseType request))

instance JiraRequest GetBoardsRequest where
  type ResponseType GetBoardsRequest = GetBoardsResponse
  makeRequest credentials baseUrl _request = callApi credentials baseUrl "rest/agile/1.0/board"

instance JiraRequest GetBoardRequest where
  type ResponseType GetBoardRequest = GetBoardResponse
  makeRequest credentials baseUrl request =
    callApi credentials baseUrl $ Url $ "rest/agile/1.0/board/" <> show (request ^. unwrap)

callApi :: (FromJSON a) => Credentials -> BaseUrl -> Url -> IO (Either String a)
callApi credentials baseUrl url = do
  request <- parseRequest (baseUrl ^. unwrap <> url ^. unwrap)
  (getResponseBody >>> eitherDecode)
    <$> httpLBS
      ( request
          & setRequestHeader "Authorization" ["Basic " <> encodedCredentials]
      )
  where
    encodedCredentials = Base64.encode $ username <> ":" <> token
    username = credentials ^. credentialsUsername . unwrap
    token = credentials ^. credentialsToken . unwrap

loadCredentials :: IO Credentials
loadCredentials = do
  loadDotEnvFile ".env"
  username <- readEnvironmentVariable "USERNAME"
  token <- readEnvironmentVariable "JIRA_TOKEN"
  pure $ Credentials username token

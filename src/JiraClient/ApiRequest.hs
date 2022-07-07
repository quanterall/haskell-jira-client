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
  makeRequest credentials baseUrl _request = callApi credentials baseUrl "rest/agile/1.0/board" []

instance JiraRequest GetBoardRequest where
  type ResponseType GetBoardRequest = GetBoardResponse
  makeRequest credentials baseUrl request = callApi credentials baseUrl url []
    where
      url = Url $ "rest/agile/1.0/board/" <> show @Integer (request ^. unwrap . unwrap)

instance JiraRequest GetBoardProjectsRequest where
  type ResponseType GetBoardProjectsRequest = GetBoardProjectsResponse
  makeRequest credentials baseUrl request = callApi credentials baseUrl url []
    where
      url = Url $ "rest/agile/1.0/board/" <> show @Integer (request ^. unwrap . unwrap) <> "/project"

instance JiraRequest GetBoardIssuesRequest where
  type ResponseType GetBoardIssuesRequest = GetBoardIssuesResponse
  makeRequest credentials baseUrl request = callApi credentials baseUrl url queryParameters
    where
      url =
        Url $
          "rest/agile/1.0/board/"
            <> show @Integer (request ^. getBoardIssuesRequestBoardId . unwrap)
            <> "/issue"
      queryParameters =
        [ ("startAt", (unStartAt >>> show >>> fromString) <$> request ^. getBoardIssuesRequestStartAt),
          ( "maxResults",
            (unMaxResults >>> show >>> fromString) <$> request ^. getBoardIssuesRequestMaxResults
          )
        ]

instance JiraRequest GetBoardSprintsRequest where
  type ResponseType GetBoardSprintsRequest = GetBoardSprintsResponse
  makeRequest credentials baseUrl request = callApi credentials baseUrl url []
    where
      url = Url $ "rest/agile/1.0/board/" <> show @Integer (request ^. unwrap . unwrap) <> "/sprint"

instance JiraRequest GetSprintIssuesRequest where
  type ResponseType GetSprintIssuesRequest = GetSprintIssuesResponse
  makeRequest credentials baseUrl request =
    callApi
      credentials
      baseUrl
      url
      []
    where
      url =
        Url $
          mconcat
            [ "rest/agile/1.0/board/",
              show @Integer (request ^. getSprintIssuesRequestBoardId . unwrap),
              "/sprint/",
              show @Integer (request ^. getSprintIssuesRequestSprintId . unwrap),
              "/issue"
            ]

instance JiraRequest GetBoardEpicsRequest where
  type ResponseType GetBoardEpicsRequest = GetBoardEpicsResponse
  makeRequest credentials baseUrl request =
    callApi
      credentials
      baseUrl
      (Url $ "rest/agile/1.0/board/" <> show @Integer (request ^. unwrap . unwrap) <> "/epic")
      []

-- GET /rest/agile/1.0/board/{boardId}/epic/{epicId}/issue
instance JiraRequest GetEpicIssuesRequest where
  type ResponseType GetEpicIssuesRequest = GetEpicIssuesResponse
  makeRequest credentials baseUrl request =
    callApi
      credentials
      baseUrl
      url
      []
    where
      url =
        Url $
          mconcat
            [ "rest/agile/1.0/board/",
              show @Integer (request ^. getEpicIssuesRequestBoardId . unwrap),
              "/epic/",
              show @Integer (request ^. getEpicIssuesRequestEpicId . unwrap),
              "/issue"
            ]

callApi :: (FromJSON a) => Credentials -> BaseUrl -> Url -> [QueryItem] -> IO (Either String a)
callApi credentials baseUrl url queryParameters = do
  request <- parseRequest (baseUrl ^. unwrap <> url ^. unwrap)
  (getResponseBody >>> eitherDecode)
    <$> httpLBS
      ( request
          & setRequestHeader "Authorization" ["Basic " <> encodedCredentials]
          & setRequestQueryString queryParameters
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

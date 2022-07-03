{-# LANGUAGE TemplateHaskell #-}

module JiraClient.Types where

import Qtility

newtype BaseUrl = BaseUrl {unBaseUrl :: String}
  deriving (Eq, Show, IsString)

newtype Url = Url {unUrl :: String}
  deriving (Eq, Show, IsString, ToJSON, FromJSON)

newtype ApiToken = ApiToken {unApiToken :: ByteString}
  deriving (Eq, Show, IsString, FromEnvironmentValue)

newtype Username = Username {unUsername :: ByteString}
  deriving (Eq, Show, IsString, FromEnvironmentValue)

data Credentials = Credentials
  { _credentialsUsername :: Username,
    _credentialsToken :: ApiToken
  }
  deriving (Eq, Show, Generic)

data GetBoardsRequest = GetBoardsRequest
  deriving (Show, Eq)

data GetBoardsResponse = GetBoardsResponse
  { _getBoardsResponseValues :: ![Board],
    _getBoardsResponseTotal :: !Int,
    _getBoardsResponseStartAt :: !Int,
    _getBoardsResponseMaxResults :: !Int,
    _getBoardsResponseIsLast :: !Bool
  }
  deriving (Show, Eq, Generic)

newtype GetBoardRequest = GetBoardRequest {unGetBoardRequest :: Integer}
  deriving (Eq, Show, Generic)

data GetBoardResponse = GetBoardResponse
  { _getBoardResponseId :: !Integer,
    _getBoardResponseLocation :: !ProjectLocation,
    _getboardResponseName :: !Text,
    _getBoardResponseSelf :: !Url
  }
  deriving (Eq, Show, Generic)

data Board = Board
  { _boardId :: !Integer,
    _boardName :: !String,
    _boardLocation :: !ProjectLocation
  }
  deriving (Eq, Show, Generic)

data ProjectLocation = ProjectLocation
  { _projectLocationDisplayName :: !Text,
    _projectLocationName :: !Text,
    _projectLocationProjectKey :: !String,
    _projectLocationProjectId :: !Integer,
    _projectLocationAvatarURI :: !Url
  }
  deriving (Eq, Show, Generic)

foldMapM makeWrapped [''BaseUrl, ''Url, ''ApiToken, ''Username, ''GetBoardRequest]

foldMapM deriveLensAndJSON [''ProjectLocation, ''Board, ''GetBoardsResponse, ''GetBoardResponse]

foldMapM makeLenses [''Credentials]

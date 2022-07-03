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

newtype BoardId = BoardId {unBoardId :: Integer}
  deriving (Eq, Show, Num, FromJSON, ToJSON)

newtype ProjectId = ProjectId {unProjectId :: Integer}
  deriving (Eq, Show, Num, FromJSON, ToJSON)

newtype IssueId = IssueId {unIssueId :: String}
  deriving (Eq, Show, IsString, FromJSON, ToJSON)

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

newtype GetBoardRequest = GetBoardRequest {unGetBoardRequest :: BoardId}
  deriving (Eq, Show, Generic)

newtype GetBoardProjectsRequest = GetBoardProjectsRequest {unGetBoardProjectsRequest :: BoardId}
  deriving (Eq, Show, Generic)

data GetBoardProjectsResponse = GetBoardProjectsResponse
  { _getBoardProjectsResponseIsLast :: !Bool,
    _getBoardProjectsResponseMaxResults :: !Int,
    _getBoardProjectsResponseStartAt :: !Int,
    _getBoardProjectsResponseTotal :: !Int,
    _getBoardProjectsResponseValues :: ![Project]
  }
  deriving (Eq, Show, Generic)

data GetBoardResponse = GetBoardResponse
  { _getBoardResponseId :: !BoardId,
    _getBoardResponseLocation :: !ProjectLocation,
    _getboardResponseName :: !Text,
    _getBoardResponseSelf :: !Url
  }
  deriving (Eq, Show, Generic)

newtype GetBoardIssuesRequest = GetBoardIssuesRequest {unGetBoardIssuesRequest :: BoardId}
  deriving (Eq, Show, Generic)

data GetBoardIssuesResponse = GetBoardIssuesResponse
  { _getBoardIssuesResponseMaxResults :: !Int,
    _getBoardIssuesResponseStartAt :: !Int,
    _getBoardIssuesResponseTotal :: !Int,
    _getBoardIssuesResponseIssues :: ![Issue],
    _getBoardIssuesResponseExpand :: !String
  }
  deriving (Eq, Show, Generic)

data Issue = Issue
  { _issueId :: !IssueId,
    _issueKey :: !Text,
    _issueSelf :: !Url,
    _issueFields :: !IssueFields
  }
  deriving (Eq, Show, Generic)

data IssueFields = IssueFields
  { _issueFieldsFlagged :: !Bool,
    _issueFieldsDescription :: !Text
  }
  deriving (Eq, Show, Generic)

data Project = Project
  { _projectAvatarUrls :: !AvatarUrls,
    _projectId :: !String,
    _projectKey :: !Text,
    _projectName :: !Text,
    _projectProjectCategory :: !ProjectCategory,
    _projectSimplified :: !Bool
  }
  deriving (Eq, Show, Generic)

data AvatarUrls = AvatarUrls
  { _avatarUrls16x16 :: !Url,
    _avatarUrls24x24 :: !Url,
    _avatarUrls32x32 :: !Url,
    _avatarUrls48x48 :: !Url
  }
  deriving (Eq, Show, Generic)

data ProjectCategory = ProjectCategory
  { _projectCategoryDescription :: !Text,
    _projectCategoryId :: !String,
    _projectCategoryName :: !Text,
    _projectCategorySelf :: !Url
  }
  deriving (Eq, Show, Generic)

data Board = Board
  { _boardId :: !BoardId,
    _boardName :: !String,
    _boardLocation :: !ProjectLocation
  }
  deriving (Eq, Show, Generic)

data ProjectLocation = ProjectLocation
  { _projectLocationDisplayName :: !Text,
    _projectLocationName :: !Text,
    _projectLocationProjectKey :: !String,
    _projectLocationProjectId :: !ProjectId,
    _projectLocationAvatarURI :: !Url
  }
  deriving (Eq, Show, Generic)

foldMapM
  makeWrapped
  [ ''BaseUrl,
    ''Url,
    ''ApiToken,
    ''Username,
    ''GetBoardRequest,
    ''GetBoardProjectsRequest,
    ''GetBoardIssuesRequest,
    ''BoardId,
    ''ProjectId,
    ''IssueId
  ]

foldMapM
  deriveLensAndJSON
  [ ''ProjectLocation,
    ''Board,
    ''AvatarUrls,
    ''Project,
    ''Issue,
    ''IssueFields,
    ''ProjectCategory,
    ''GetBoardsResponse,
    ''GetBoardResponse,
    ''GetBoardProjectsRequest,
    ''GetBoardProjectsResponse,
    ''GetBoardIssuesResponse
  ]

foldMapM makeLenses [''Credentials]

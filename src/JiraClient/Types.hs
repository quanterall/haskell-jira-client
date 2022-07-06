{-# LANGUAGE TemplateHaskell #-}

module JiraClient.Types where

import Qtility
import RIO.Time (UTCTime)

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

newtype SprintId = SprintId {unSprintId :: Integer}
  deriving (Eq, Show, Num, FromJSON, ToJSON)

newtype CommentId = CommentId {unCommentId :: String}
  deriving (Eq, Show, IsString, FromJSON, ToJSON)

newtype AccountId = AccountId {unAccountId :: String}
  deriving (Eq, Show, IsString, FromJSON, ToJSON)

newtype WorkLogId = WorkLogId {unWorkLogId :: String}
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
    _getBoardIssuesResponseTotal :: Int,
    _getBoardIssuesResponseIssues :: ![Issue],
    _getBoardIssuesResponseExpand :: !String
  }
  deriving (Eq, Show, Generic)

newtype GetBoardSprintsRequest = GetBoardSprintsRequest {unGetBoardSprintsRequest :: BoardId}
  deriving (Eq, Show, Generic)

data GetBoardSprintsResponse = GetBoardSprintsResponse
  { _getBoardSprintsResponseMaxResults :: !Int,
    _getBoardSprintsResponseStartAt :: !Int,
    _getBoardSprintsResponseTotal :: !(Maybe Int),
    _getBoardSprintsResponseValues :: ![Sprint]
  }
  deriving (Eq, Show, Generic)

data Sprint = Sprint
  { _sprintId :: !SprintId,
    _sprintSelf :: !Url,
    _sprintState :: !Text,
    _sprintName :: !Text,
    _sprintStartDate :: !UTCTime,
    _sprintEndDate :: !UTCTime,
    _sprintCompleteDate :: !(Maybe UTCTime),
    _sprintOriginBoardId :: !BoardId,
    _sprintGoal :: !Text
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
    _issueFieldsSprint :: !(Maybe BasicSprintInfo),
    _issueFieldsClosedSprint :: !(Maybe [Sprint]),
    _issueFieldsDescription :: !Text,
    _issueFieldsProject :: !Project,
    _issueFieldsComment :: !CommentsInfo,
    _issueFieldsEpic :: !(Maybe BasicEpicInfo),
    _issueFieldsWorklog :: !WorkLogInfo,
    _issueFieldsUpdated :: !UTCTime,
    _issueFieldsTimetracking :: !TimeTrackingInfo
  }
  deriving (Eq, Show, Generic)

data TimeTrackingInfo = TimeTrackingInfo
  { _timeTrackingInfoRemainingEstimate :: !(Maybe Text),
    _timeTrackingInfoRemainingEstimateSeconds :: !(Maybe Integer),
    _timeTrackingInfoTimeSpent :: !(Maybe Text),
    _timeTrackingInfoTimeSpentSeconds :: !(Maybe Integer)
  }
  deriving (Eq, Show, Generic)

data WorkLogInfo = WorkLogInfo
  { _workLogInfoMaxResults :: !Int,
    _workLogInfoStartAt :: !Int,
    _workLogInfoTotal :: !Int,
    _workLogInfoWorklogs :: ![WorkLogItem]
  }
  deriving (Eq, Show, Generic)

data WorkLogItem = WorkLogItem
  { _workLogItemAuthor :: !User,
    _workLogItemCreated :: !UTCTime,
    _workLogItemId :: !WorkLogId,
    _workLogItemIssueId :: !IssueId,
    _workLogItemSelf :: !Url,
    _workLogItemStarted :: !UTCTime,
    _workLogItemTimeSpent :: !Text,
    _workLogItemTimeSpent :: !Text,
    _workLogItemTimeSpentSeconds :: !Integer,
    _workLogItemUpdated :: !(Maybe UTCTime),
    _workLogItemUpdateAuthor :: !(Maybe User)
  }
  deriving (Eq, Show, Generic)

data User = User
  { _userAccountId :: !AccountId,
    _userAccountType :: !Text,
    _userActive :: !Bool,
    _userAvatarUrls :: !AvatarUrls,
    _userDisplayName :: !Text,
    _userEmailAddress :: !Text,
    _userSelf :: !Url,
    _userTimeZone :: !Text
  }
  deriving (Eq, Show, Generic)

data CommentsInfo = CommentsInfo
  { _commentsInfoComments :: ![Comment],
    _commentsInfoMaxResults :: !Int,
    _commentsInfoStartAt :: !Int,
    _commentsInfoTotal :: !Int,
    _commentsInfoSelf :: !Url
  }
  deriving (Eq, Show, Generic)

data BasicSprintInfo = BasicSprintInfo
  { _basicSprintInfoId :: !SprintId,
    _basicSprintInfoSelf :: !Url,
    _basicSprintInfoState :: !Text,
    _basicSprintInfoName :: !Text,
    _basicSprintInfoGoal :: !Text
  }
  deriving (Eq, Show, Generic)

data BasicUserInfo = BasicUserInfo
  { _basicUserInfoSelf :: !Url,
    _basicUserInfoAccountId :: !AccountId,
    _basicUserInfoDisplayName :: !Text,
    _basicUserInfoActive :: !Bool
  }
  deriving (Eq, Show, Generic)

data BasicEpicInfo = BasicEpicInfo
  { _basicEpicInfoId :: !IssueId,
    _basicEpicInfoSelf :: !Url,
    _basicEpicInfoName :: !Text,
    _basicEpicInfoSummary :: !Text,
    _basicEpicInfoDone :: !Bool
  }
  deriving (Eq, Show, Generic)

data Comment = Comment
  { _commentSelf :: !Url,
    _commentId :: !CommentId,
    _commentAuthor :: !BasicUserInfo,
    -- @TODO: Figure out what this field needs to be, as it seems very dynamic.
    _commentBody :: !Value,
    _commentUpdateAuthor :: !(Maybe BasicUserInfo),
    _commentCreated :: !UTCTime,
    _commentUpdated :: !(Maybe UTCTime),
    _commentVisibility :: !(Maybe CommentVisibility)
  }
  deriving (Eq, Show, Generic)

data CommentBody = CommentBody
  { _commentBodyType :: !Text,
    _commentBodyVersion :: !Int,
    _commentBodyContent :: ![CommentBodyContent]
  }
  deriving (Eq, Show, Generic)

data CommentBodyContent = CommentBodyContent
  { _commentBodyContentType :: !Text,
    _commentBodyContentText :: !Text
  }
  deriving (Eq, Show, Generic)

data CommentVisibility = CommentVisibility
  { _commentVisibilityType :: !Text,
    _commentVisibilityValue :: !Text,
    _commentVisibilityIdentifier :: !Text
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
    ''GetBoardSprintsRequest,
    ''BoardId,
    ''ProjectId,
    ''IssueId,
    ''SprintId,
    ''CommentId,
    ''AccountId
  ]

foldMapM
  deriveLensAndJSON
  [ ''ProjectLocation,
    ''Board,
    ''AvatarUrls,
    ''Project,
    ''Issue,
    ''Sprint,
    ''CommentsInfo,
    ''Comment,
    ''User,
    ''WorkLogInfo,
    ''WorkLogItem,
    ''TimeTrackingInfo,
    ''CommentVisibility,
    ''CommentBody,
    ''CommentBodyContent,
    ''BasicUserInfo,
    ''BasicSprintInfo,
    ''BasicEpicInfo,
    ''IssueFields,
    ''ProjectCategory,
    ''GetBoardsResponse,
    ''GetBoardResponse,
    ''GetBoardProjectsRequest,
    ''GetBoardProjectsResponse,
    ''GetBoardIssuesResponse,
    ''GetBoardSprintsResponse
  ]

foldMapM makeLenses [''Credentials]

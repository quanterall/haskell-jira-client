{-# LANGUAGE TemplateHaskell #-}

module JiraClient.Types where

import Data.Aeson (withText)
import Data.OpenApi (ToSchema)
import Qtility
import RIO.Time (UTCTime)

newtype StartAt = StartAt {unStartAt :: Int}
  deriving (Eq, Show, Ord, Num, ToJSON, FromJSON, ToSchema)

newtype MaxResults = MaxResults {unMaxResults :: Int}
  deriving (Eq, Show, Ord, Num, ToJSON, FromJSON, ToSchema)

newtype BaseUrl = BaseUrl {unBaseUrl :: String}
  deriving (Eq, Show, Ord, IsString, ToJSON, FromJSON, ToSchema)

newtype Url = Url {unUrl :: String}
  deriving (Eq, Show, Ord, IsString, ToJSON, FromJSON, ToSchema)

newtype JiraToken = JiraToken {unJiraToken :: ByteString}
  deriving (Eq, Show, Ord, IsString, FromEnvironmentValue, Generic, ToSchema)

instance FromJSON JiraToken where
  parseJSON = withText "JiraToken" $ encodeUtf8 >>> JiraToken >>> pure

instance ToJSON JiraToken where
  toJSON = unJiraToken >>> decodeUtf8Lenient >>> toJSON

newtype JiraUsername = JiraUsername {unJiraUsername :: ByteString}
  deriving (Eq, Show, Ord, IsString, FromEnvironmentValue, Generic, ToSchema)

instance FromJSON JiraUsername where
  parseJSON = withText "JiraUsername" $ encodeUtf8 >>> JiraUsername >>> pure

instance ToJSON JiraUsername where
  toJSON = unJiraUsername >>> decodeUtf8Lenient >>> toJSON

newtype BoardId = BoardId {unBoardId :: Integer}
  deriving (Eq, Show, Ord, Num, FromJSON, ToJSON, ToSchema)

newtype ProjectId = ProjectId {unProjectId :: Integer}
  deriving (Eq, Show, Ord, Num, FromJSON, ToJSON, ToSchema)

newtype IssueId = IssueId {unIssueId :: String}
  deriving (Eq, Show, Ord, IsString, FromJSON, ToJSON, ToSchema)

newtype SprintId = SprintId {unSprintId :: Integer}
  deriving (Eq, Show, Ord, Num, FromJSON, ToJSON, ToSchema)

newtype CommentId = CommentId {unCommentId :: String}
  deriving (Eq, Show, Ord, IsString, FromJSON, ToJSON, ToSchema)

newtype AccountId = AccountId {unAccountId :: String}
  deriving (Eq, Show, Ord, IsString, FromJSON, ToJSON, ToSchema)

newtype WorkLogId = WorkLogId {unWorkLogId :: String}
  deriving (Eq, Show, Ord, IsString, FromJSON, ToJSON, ToSchema)

newtype EpicId = EpicId {unEpicId :: Int}
  deriving (Eq, Show, Ord, Num, FromJSON, ToJSON, ToSchema)

data Credentials = Credentials
  { _credentialsUsername :: !JiraUsername,
    _credentialsToken :: !JiraToken
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

data GetBoardIssuesRequest = GetBoardIssuesRequest
  { _getBoardIssuesRequestBoardId :: !BoardId,
    _getBoardIssuesRequestStartAt :: !(Maybe StartAt),
    _getBoardIssuesRequestMaxResults :: !(Maybe MaxResults)
  }
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

data GetSprintIssuesRequest = GetSprintIssuesRequest
  { _getSprintIssuesRequestBoardId :: !BoardId,
    _getSprintIssuesRequestSprintId :: !SprintId
  }
  deriving (Eq, Show, Generic)

data GetSprintIssuesResponse = GetSprintIssuesResponse
  { _getSprintIssuesResponseMaxResults :: !Int,
    _getSprintIssuesResponseStartAt :: !Int,
    _getSprintIssuesResponseTotal :: !Int,
    _getSprintIssuesResponseIssues :: ![Issue]
  }
  deriving (Eq, Show, Generic)

data GetBoardSprintsResponse = GetBoardSprintsResponse
  { _getBoardSprintsResponseMaxResults :: !Int,
    _getBoardSprintsResponseStartAt :: !Int,
    _getBoardSprintsResponseTotal :: !(Maybe Int),
    _getBoardSprintsResponseValues :: ![Sprint]
  }
  deriving (Eq, Show, Generic)

newtype GetBoardEpicsRequest = GetBoardEpicsRequest {unGetBoardEpicsRequest :: BoardId}
  deriving (Eq, Show, Generic)

data GetBoardEpicsResponse = GetBoardEpicsResponse
  { _getBoardEpicsResponseMaxResults :: !Int,
    _getBoardEpicsResponseStartAt :: !Int,
    _getBoardEpicsResponseTotal :: !(Maybe Int),
    _getBoardEpicsResponseIsLast :: !Bool,
    _getBoardEpicsResponseValues :: !(Maybe [Epic])
  }
  deriving (Eq, Show, Generic)

data Epic = Epic
  { _epicId :: !EpicId,
    _epicSelf :: !Url,
    _epicName :: !Text,
    _epicSummary :: !Text,
    _epicColor :: !Color,
    _epicDone :: !Bool
  }
  deriving (Eq, Show, Generic)

newtype Color = Color {_colorKey :: Text}
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
    _issueFieldsDescription :: !(Maybe Text),
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
    _userEmailAddress :: !(Maybe Text),
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
  { _basicEpicInfoId :: !EpicId,
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
    _projectProjectCategory :: !(Maybe ProjectCategory),
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
    ''JiraToken,
    ''JiraUsername,
    ''GetBoardRequest,
    ''GetBoardProjectsRequest,
    ''GetBoardSprintsRequest,
    ''GetBoardEpicsRequest,
    ''BoardId,
    ''ProjectId,
    ''IssueId,
    ''SprintId,
    ''CommentId,
    ''AccountId,
    ''StartAt,
    ''MaxResults
  ]

foldMapM
  deriveLensAndJSON
  [ ''ProjectLocation,
    ''Board,
    ''AvatarUrls,
    ''Project,
    ''Issue,
    ''Sprint,
    ''Epic,
    ''CommentsInfo,
    ''Comment,
    ''User,
    ''Color,
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
    ''GetBoardSprintsResponse,
    ''GetSprintIssuesRequest,
    ''GetSprintIssuesResponse,
    ''GetBoardEpicsResponse,
    ''GetBoardIssuesRequest
  ]

foldMapM makeLenses [''Credentials]

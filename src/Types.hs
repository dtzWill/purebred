{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | Basic types for the UI used by this library
module Types
  ( module Types
  , Tag
  ) where

import Data.Semigroup (Semigroup, (<>))
import qualified Brick.AttrMap as Brick
import qualified Brick.Focus as Brick
import Brick.Widgets.Core ((<=>), emptyWidget)
import Brick.Types (EventM, Next, Widget)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Control.Lens
import qualified Data.Map as Map
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Graphics.Vty.Input.Events as Vty
import Data.Time (UTCTime)
import qualified Data.CaseInsensitive as CI
import qualified Network.Mail.Mime as Mail

import Notmuch (Tag)
import Data.MIME

import Error

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}

-- | Used to identify widgets in brick
data Name =
    SearchThreadsEditor
    | ListOfMails
    | ListOfThreads
    | ScrollingMailView
    | ComposeFrom
    | ComposeTo
    | ComposeSubject
    | ScrollingHelpView
    | ManageMailTagsEditor
    | ManageThreadTagsEditor
    | ListOfAttachments
    | StatusBar
    deriving (Eq,Show,Ord)

-- | Drawing types
newtype VBox = VBox { unVBox :: Widget Name }

instance Semigroup VBox where
  VBox a <> VBox b = VBox (a <=> b)

instance Monoid VBox where
  mappend = (<>)
  mempty = VBox emptyWidget

{- | main application interface

The main UI shows a list of e-mails, allows the user to manipulate the notmuch
search and composes e-mails from here.

-}
data MailIndex = MailIndex
    { _miListOfMails  :: L.List Name NotmuchMail
    , _miListOfThreads :: L.List Name NotmuchThread
    , _miSearchThreadsEditor :: E.Editor T.Text Name
    , _miMailTagsEditor :: E.Editor T.Text Name
    , _miThreadTagsEditor :: E.Editor T.Text Name
    }

miListOfMails :: Lens' MailIndex (L.List Name NotmuchMail)
miListOfMails = lens _miListOfMails (\m v -> m { _miListOfMails = v })

miListOfThreads :: Lens' MailIndex (L.List Name NotmuchThread)
miListOfThreads = lens _miListOfThreads (\m v -> m { _miListOfThreads = v})

miSearchThreadsEditor :: Lens' MailIndex (E.Editor T.Text Name)
miSearchThreadsEditor = lens _miSearchThreadsEditor (\m v -> m { _miSearchThreadsEditor = v})

miMailTagsEditor :: Lens' MailIndex (E.Editor T.Text Name)
miMailTagsEditor = lens _miMailTagsEditor (\m v -> m { _miMailTagsEditor = v})

miThreadTagsEditor :: Lens' MailIndex (E.Editor T.Text Name)
miThreadTagsEditor = lens _miThreadTagsEditor (\m v -> m { _miThreadTagsEditor = v})

data HeadersState = ShowAll | Filtered

data MailView = MailView
    { _mvMail :: Maybe MIMEMessage
    , _mvHeadersState :: HeadersState
    }

mvMail :: Lens' MailView (Maybe MIMEMessage)
mvMail = lens _mvMail (\mv pm -> mv { _mvMail = pm })

mvHeadersState :: Lens' MailView HeadersState
mvHeadersState = lens _mvHeadersState (\mv hs -> mv { _mvHeadersState = hs })

data Compose = Compose
    { _cMail :: Mail.Mail
    , _cFrom :: E.Editor T.Text Name
    , _cTo :: E.Editor T.Text Name
    , _cSubject :: E.Editor T.Text Name
    , _cAttachments :: L.List Name Mail.Part
    }

cMail :: Lens' Compose Mail.Mail
cMail = lens _cMail (\c x -> c { _cMail = x })

cFrom :: Lens' Compose (E.Editor T.Text Name)
cFrom = lens _cFrom (\c x -> c { _cFrom = x })

cTo :: Lens' Compose (E.Editor T.Text Name)
cTo = lens _cTo (\c x -> c { _cTo = x })

cSubject :: Lens' Compose (E.Editor T.Text Name)
cSubject = lens _cSubject (\c x -> c { _cSubject = x })

cAttachments :: Lens' Compose (L.List Name Mail.Part)
cAttachments = lens _cAttachments (\c x -> c { _cAttachments = x })

data NotmuchSettings a = NotmuchSettings
    { _nmSearch :: T.Text
    , _nmDatabase :: a
    , _nmNewTag :: Tag
    }

nmSearch :: Lens' (NotmuchSettings a) T.Text
nmSearch f (NotmuchSettings a b c) = fmap (\a' -> NotmuchSettings a' b c) (f a)

nmDatabase :: Lens (NotmuchSettings a) (NotmuchSettings b) a b
nmDatabase f (NotmuchSettings a b c) = fmap (\b' -> NotmuchSettings a b' c) (f b)

nmNewTag :: Getter (NotmuchSettings a) Tag
nmNewTag = to (\(NotmuchSettings _ _ c) -> c)

data Configuration a b = Configuration
    { _confColorMap :: Brick.AttrMap
    , _confNotmuch :: NotmuchSettings a
    , _confEditor :: b
    , _confMailView :: MailViewSettings
    , _confIndexView :: IndexViewSettings
    , _confComposeView :: ComposeViewSettings
    , _confHelpView :: HelpViewSettings
    , _confDefaultView :: ViewName
    }

type UserConfiguration = Configuration (IO FilePath) (IO String)
type InternalConfiguration = Configuration FilePath String

confColorMap :: Getter (Configuration a b) Brick.AttrMap
confColorMap = to (\(Configuration a _ _ _ _ _ _ _) -> a)

confEditor :: Lens (Configuration a b) (Configuration a b') b b'
confEditor f (Configuration a b c d e g h i) = fmap (\c' -> Configuration a b c' d e g h i) (f c)

confNotmuch :: Lens (Configuration a c) (Configuration b c) (NotmuchSettings a) (NotmuchSettings b)
confNotmuch f (Configuration a b c d e g h i) = fmap (\b' -> Configuration a b' c d e g h i) (f b)

confMailView :: Lens' (Configuration a b) MailViewSettings
confMailView f (Configuration a b c d e g h i) = fmap (\d' -> Configuration a b c d' e g h i) (f d)

confIndexView :: Lens' (Configuration a b) IndexViewSettings
confIndexView f (Configuration a b c d e g h i) = fmap (\e' -> Configuration a b c d e' g h i) (f e)

confComposeView :: Lens' (Configuration a b) ComposeViewSettings
confComposeView f (Configuration a b c d e g h i) = fmap (\g' -> Configuration a b c d e g' h i) (f g)

confHelpView :: Lens' (Configuration a b) HelpViewSettings
confHelpView f (Configuration a b c d e g h i) = fmap (\h' -> Configuration a b c d e g h' i) (f h)

confDefaultView :: Lens' (Configuration a b) ViewName
confDefaultView = lens _confDefaultView (\conf x -> conf { _confDefaultView = x })

data ComposeViewSettings = ComposeViewSettings
    { _cvFromKeybindings :: [Keybinding 'ComposeView 'ComposeFrom (Next AppState)]
    , _cvToKeybindings :: [Keybinding 'ComposeView 'ComposeTo (Next AppState)]
    , _cvSubjectKeybindings :: [Keybinding 'ComposeView 'ComposeSubject (Next AppState)]
    , _cvSendMailCmd :: Mail.Mail -> IO ()
    , _cvListOfAttachmentsKeybindings :: [Keybinding 'ComposeView 'ListOfAttachments (Next AppState)]
    }

cvFromKeybindings :: Lens' ComposeViewSettings [Keybinding 'ComposeView 'ComposeFrom (Next AppState)]
cvFromKeybindings = lens _cvFromKeybindings (\cv x -> cv { _cvFromKeybindings = x })

cvToKeybindings :: Lens' ComposeViewSettings [Keybinding 'ComposeView 'ComposeTo (Next AppState)]
cvToKeybindings = lens _cvToKeybindings (\cv x -> cv { _cvToKeybindings = x })

cvSubjectKeybindings :: Lens' ComposeViewSettings [Keybinding 'ComposeView 'ComposeSubject (Next AppState)]
cvSubjectKeybindings = lens _cvSubjectKeybindings (\cv x -> cv { _cvSubjectKeybindings = x })

cvSendMailCmd :: Lens' ComposeViewSettings (Mail.Mail -> IO ())
cvSendMailCmd = lens _cvSendMailCmd (\cv x -> cv { _cvSendMailCmd = x })

cvListOfAttachmentsKeybindings :: Lens' ComposeViewSettings [Keybinding 'ComposeView 'ListOfAttachments (Next AppState)]
cvListOfAttachmentsKeybindings = lens _cvListOfAttachmentsKeybindings (\cv x -> cv { _cvListOfAttachmentsKeybindings = x })

newtype HelpViewSettings = HelpViewSettings
  { _hvKeybindings :: [Keybinding 'Help 'ScrollingHelpView (Next AppState)]
  }

hvKeybindings :: Lens' HelpViewSettings [Keybinding 'Help 'ScrollingHelpView (Next AppState)]
hvKeybindings f (HelpViewSettings a) = fmap (\a' -> HelpViewSettings a') (f a)

data IndexViewSettings = IndexViewSettings
    { _ivBrowseThreadsKeybindings :: [Keybinding 'Threads 'ListOfThreads (Next AppState)]
    , _ivBrowseMailsKeybindings :: [Keybinding 'Mails 'ListOfMails (Next AppState)]
    , _ivSearchThreadsKeybindings :: [Keybinding 'Threads 'SearchThreadsEditor (Next AppState)]
    , _ivManageMailTagsKeybindings :: [Keybinding 'Mails 'ManageMailTagsEditor (Next AppState)]
    , _ivManageThreadTagsKeybindings :: [Keybinding 'Threads 'ManageThreadTagsEditor (Next AppState)]
    }

ivBrowseThreadsKeybindings :: Lens' IndexViewSettings [Keybinding 'Threads 'ListOfThreads (Next AppState)]
ivBrowseThreadsKeybindings f (IndexViewSettings a b c d e) = fmap (\a' -> IndexViewSettings a' b c d e) (f a)

ivBrowseMailsKeybindings :: Lens' IndexViewSettings [Keybinding 'Mails 'ListOfMails (Next AppState)]
ivBrowseMailsKeybindings f (IndexViewSettings a b c d e) = fmap (\b' -> IndexViewSettings a b' c d e) (f b)

ivSearchThreadsKeybindings :: Lens' IndexViewSettings [Keybinding 'Threads 'SearchThreadsEditor (Next AppState)]
ivSearchThreadsKeybindings f (IndexViewSettings a b c d e) = fmap (\c' -> IndexViewSettings a b c' d e) (f c)

ivManageMailTagsKeybindings :: Lens' IndexViewSettings [Keybinding 'Mails 'ManageMailTagsEditor (Next AppState)]
ivManageMailTagsKeybindings f (IndexViewSettings a b c d e) = fmap (\d' -> IndexViewSettings a b c d' e) (f d)

ivManageThreadTagsKeybindings :: Lens' IndexViewSettings [Keybinding 'Threads 'ManageThreadTagsEditor (Next AppState)]
ivManageThreadTagsKeybindings f (IndexViewSettings a b c d e) = fmap (\e' -> IndexViewSettings a b c d e') (f e)

data MailViewSettings = MailViewSettings
    { _mvIndexRows           :: Int
    , _mvPreferedContentType :: ContentType
    , _mvHeadersToShow       :: CI.CI B.ByteString -> Bool
    , _mvKeybindings         :: [Keybinding 'ViewMail 'ScrollingMailView (Next AppState)]
    }

mvIndexRows :: Lens' MailViewSettings Int
mvIndexRows f (MailViewSettings a b c d) = fmap (\a' -> MailViewSettings a' b c d) (f a)

mvPreferredContentType :: Lens' MailViewSettings ContentType
mvPreferredContentType f (MailViewSettings a b c d) = fmap (\b' -> MailViewSettings a b' c d) (f b)

mvHeadersToShow :: Getter MailViewSettings (CI.CI B.ByteString -> Bool)
mvHeadersToShow = to (\(MailViewSettings _ _ h _) -> h)

mvKeybindings :: Lens' MailViewSettings [Keybinding 'ViewMail 'ScrollingMailView (Next AppState)]
mvKeybindings f (MailViewSettings a b c d) = fmap (\d' -> MailViewSettings a b c d') (f d)

data ViewName
    = Threads
    | Mails
    | ViewMail
    | ComposeView
    | Help
    deriving (Eq,Ord,Show)

data View = View
    { _vFocus :: Brick.FocusRing Name
    , _vWidgets :: [Name]
    }

vWidgets :: Lens' View [Name]
vWidgets = lens _vWidgets (\settings x -> settings { _vWidgets = x })

vFocus :: Lens' View (Brick.FocusRing Name)
vFocus = lens _vFocus (\settings x -> settings { _vFocus = x})

data ViewSettings = ViewSettings
    { _vsViews :: Map.Map ViewName View
    , _vsFocusedView :: Brick.FocusRing ViewName
    }

vsViews :: Lens' ViewSettings (Map.Map ViewName View)
vsViews = lens _vsViews (\settings x -> settings { _vsViews = x })

vsFocusedView :: Lens' ViewSettings (Brick.FocusRing ViewName)
vsFocusedView = lens _vsFocusedView (\settings x -> settings { _vsFocusedView = x})

-- | Overall application state
data AppState = AppState
    { _asConfig    :: InternalConfiguration
    , _asMailIndex :: MailIndex
    , _asMailView  :: MailView
    , _asCompose   :: Compose  -- ^ state to keep when user creates a new mail
    , _asError     :: Maybe Error -- ^ in case of errors, show this error message
    , _asViews     :: ViewSettings -- ^ stores widget and focus information
    }

asConfig :: Lens' AppState InternalConfiguration
asConfig = lens _asConfig (\appstate x -> appstate { _asConfig = x })

asMailIndex :: Lens' AppState MailIndex
asMailIndex = lens _asMailIndex (\appstate x -> appstate { _asMailIndex = x })

asMailView :: Lens' AppState MailView
asMailView = lens _asMailView (\appstate x -> appstate { _asMailView = x })

asCompose :: Lens' AppState Compose
asCompose = lens _asCompose (\appstate x -> appstate { _asCompose = x })

asError :: Lens' AppState (Maybe Error)
asError = lens _asError (\appstate x -> appstate { _asError = x })

asViews :: Lens' AppState ViewSettings
asViews f (AppState a b c d e g) = fmap (\g' -> AppState a b c d e g') (f g)

data Action (v :: ViewName) (ctx :: Name) a = Action
    { _aDescription :: String
    , _aAction :: AppState -> EventM Name a
    }

aAction :: Getter (Action v ctx a) (AppState -> EventM Name a)
aAction = to (\(Action _ b) -> b)

data Keybinding (v :: ViewName) (ctx :: Name) a = Keybinding
    { _kbEvent :: Vty.Event
    , _kbAction :: Action v ctx a
    }
instance Eq (Keybinding v ctx a) where
  (==) (Keybinding a _) (Keybinding b _) = a == b
  (/=) (Keybinding a _) (Keybinding b _) = a /= b

kbEvent :: Getter (Keybinding v ctx a) Vty.Event
kbEvent = to (\(Keybinding b _) -> b)

kbAction :: Getter (Keybinding v ctx a) (Action v ctx a)
kbAction = to (\(Keybinding _ c) -> c)

aDescription :: Getter (Action v ctx a) String
aDescription = to (\(Action a _ ) -> a)

type Body = T.Text
type Header = T.Text

-- | an email from the notmuch database
data NotmuchMail = NotmuchMail
    { _mailSubject :: T.Text
    , _mailFrom :: T.Text
    , _mailDate :: UTCTime
    , _mailTags :: [Tag]
    , _mailId :: B.ByteString
    } deriving (Show, Eq)

mailSubject :: Lens' NotmuchMail T.Text
mailSubject = lens _mailSubject (\m s -> m { _mailSubject = s })

mailFrom :: Lens' NotmuchMail T.Text
mailFrom = lens _mailFrom (\m f -> m { _mailFrom = f })

mailDate :: Lens' NotmuchMail UTCTime
mailDate = lens _mailDate (\m d -> m { _mailDate = d })

mailTags :: Lens' NotmuchMail [Tag]
mailTags = lens _mailTags (\m t -> m { _mailTags = t })

mailId :: Lens' NotmuchMail B.ByteString
mailId = lens _mailId (\m i -> m { _mailId = i })

data NotmuchThread = NotmuchThread
    { _thSubject :: T.Text
    , _thAuthors :: [T.Text]
    , _thDate :: UTCTime
    , _thTags :: [Tag]
    , _thReplies :: Int
    , _thId :: B.ByteString
    } deriving (Show, Eq)

thSubject :: Lens' NotmuchThread T.Text
thSubject = lens _thSubject (\m s -> m { _thSubject = s })

thAuthors :: Lens' NotmuchThread [T.Text]
thAuthors = lens _thAuthors (\m f -> m { _thAuthors = f })

thDate :: Lens' NotmuchThread UTCTime
thDate = lens _thDate (\m d -> m { _thDate = d })

thTags :: Lens' NotmuchThread [Tag]
thTags = lens _thTags (\m t -> m { _thTags = t })

thReplies :: Lens' NotmuchThread Int
thReplies = lens _thReplies (\m t -> m { _thReplies = t })

thId :: Lens' NotmuchThread B.ByteString
thId = lens _thId (\m t -> m { _thId = t })

-- | Utility for safe conversion from bytestring to text
decodeLenient :: B.ByteString -> T.Text
decodeLenient = T.decodeUtf8With T.lenientDecode

-- | Tag operations
data TagOp = RemoveTag Tag | AddTag Tag | ResetTags
  deriving (Show, Eq)

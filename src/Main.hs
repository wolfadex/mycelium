{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Main where


import           Control.Monad.IO.Class            (liftIO)
import           Data.Aeson
import           GHC.Generics                      (Generic)
import           Network.HTTP.Types                as Http
import           Network.Wai                       (Application, responseFile)
import           Network.Wai.Handler.Warp          (run)
import           Servant                           ((:<|>) (..), (:>), Capture,
                                                    Get, Handler, JSON, Post,
                                                    Proxy (..), QueryParam, Raw,
                                                    ReqBody, Server, ServerT)
import qualified Servant
import qualified System.Directory
import           System.Directory.Internal.Prelude as System.Directory
import           System.FilePath                   ((</>))
import           System.Process                    (CreateProcess (..))
import qualified System.Process

import           WaiAppStatic.Storage.Filesystem   (defaultWebAppSettings)
import           WaiAppStatic.Types                (StaticSettings (..))


main :: IO ()
main = run 2222 app


app :: Application
app = Servant.serve routes server


routes :: Proxy Routes
routes = Proxy


type Routes = API :<|> Static :<|> ServeSpa


type API = "api" :>
  (    ProjectsAPI
  :<|> "listfolders" :> QueryParam "path" FilePath :> Get '[JSON] [FilePath]
  )


type ProjectsAPI = "projects" :>
    (    Get '[JSON] [Project]
    :<|> ReqBody '[JSON] NewProject :> Post '[JSON] Project
    )


type Static = "static" :> Raw


type ServeSpa = Raw


server :: Server Routes
server = handleApi :<|> getStatic :<|> serveSpa


serveIndex :: Application
serveIndex _ respond =
    respond $ responseFile Http.ok200 [(hContentType, "text/html")] (spaPath  <> "/index.html") Nothing


getStatic :: ServerT Raw m
getStatic = Servant.serveDirectoryFileServer "src/static"


handleApi :: (Handler [Project] :<|> (NewProject -> Handler Project)) :<|> (Maybe FilePath -> Handler [FilePath])
handleApi = handleProjects :<|> getFolderList

handleProjects :: Handler [Project] :<|> (NewProject -> Handler Project)
handleProjects = getProjects :<|> createProject


createProject :: NewProject -> Handler Project
createProject (NewProject { newProjectPath = path, newProjectName = name }) = do
  let newPath = path </> name
  () <- liftIO $ System.Directory.createDirectoryIfMissing True newPath
  (maybeStdin, mayebStdout, maybeStderr, notSure) <- liftIO $ System.Process.createProcess $ (System.Process.shell "yes | elm init") { cwd = Just newPath }
  _ <- liftIO $ print newPath
  _ <- liftIO $ print (show maybeStdin)
  _ <- liftIO $ print (show mayebStdout)
  _ <- liftIO $ print (show maybeStderr)
  return $ Project { projectPath = newPath, favorited = False }


getFolderList :: Maybe FilePath -> Handler [FilePath]
getFolderList maybePath = liftIO $ do
  home <- System.Directory.getHomeDirectory
  let rootPath = maybe home ((</>) home) maybePath
  childPaths <- System.Directory.listDirectory rootPath
  return $ formatFolders rootPath childPaths


formatFolders :: FilePath -> [FilePath] -> [FilePath]
formatFolders parent = map (\p -> parent </> p)


getProjects :: Handler [Project]
getProjects = do
  projectsPath <- liftIO $ System.Directory.getAppUserDataDirectory "mycelium/projects.json"
  exists <- liftIO $ System.Directory.doesFileExist projectsPath
  if exists
      then liftIO $ readFile projectsPath >>= return . decodeProjects
      else liftIO $ return []



serveSpa :: ServerT Raw m
serveSpa = Servant.serveDirectoryWith ((defaultWebAppSettings spaPath) {ss404Handler = Just serveIndex})


spaPath :: String
spaPath = "src/public"


data Project = Project
  { projectPath :: FilePath
  , favorited   :: Bool
  } deriving (Eq, Read, Show, Generic)

instance FromJSON Project
instance ToJSON Project


decodeProjects :: String -> [Project]
decodeProjects = read


data NewProject = NewProject
  { newProjectPath :: FilePath
  , newProjectName :: String
  } deriving (Eq, Generic)

instance FromJSON NewProject

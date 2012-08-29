{-# LANGUAGE TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, TypeFamilies, OverloadedStrings #-}
module Application where

import Control.Applicative
import Yesod
import Yesod.Form.Jquery
import Text.Hamlet
import qualified Data.Text as T
import Control.Concurrent.STM

             

app :: IO Application
app = do
  dents <- liftIO $ atomically $ newTVar []
  toWaiAppPlain $ ChatApp dents


data Dent = Dent
               { dentSender :: T.Text 
               , dentBody :: T.Text
               }
             
data ChatApp = ChatApp { chatDents :: TVar [Dent] }

getDents = chatDents <$> getYesod

mkYesod "ChatApp" [parseRoutes|
                   / HomeR GET
                   /send SendR POST
                   |]

instance Yesod ChatApp where
    defaultLayout widget =
        do pc <- widgetToPageContent widget
           hamletToRepHtml $(hamletFile "default-layout.hamlet")

instance YesodJquery ChatApp

instance RenderMessage ChatApp FormMessage where
    renderMessage _ _ = defaultFormMessage


getHomeR :: Handler RepHtml
getHomeR = do
  (form, enctype) <- generateFormPost dentForm
  dents <- getDents >>=
           liftIO . atomically . readTVar
  defaultLayout $ do 
    setTitle "Welcome to Yitter"
    [whamlet|
     <h1>Chat
     <form method=POST action=@{SendR} enctype=#{enctype}>
       ^{form}
       <input type=submit value=Ok>
     $forall dent <- dents
       <article>
         <h2>#{dentSender dent}
         <p>#{dentBody dent}
     |]
             
dentForm = renderDivs $ Dent
           <$> areq textField "Name: " Nothing
           <*> areq textField "Body: " Nothing

postSendR :: Handler RepHtml
postSendR = do
  ((result, widget), enctype) <- runFormPost dentForm
  case result of
    FormSuccess dent -> do
                dents <- getDents
                liftIO $ atomically $
                       modifyTVar dents $ (dent :)
                redirect HomeR
    _ ->
      defaultLayout [whamlet|
                     <h1>Oops
                     |]


-- TODO: i18n, form validation
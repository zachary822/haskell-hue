{-# LANGUAGE CApiFFI #-}

module Lib.HueBridge.Discovery where

import Control.Monad
import Data.Char
import Data.IORef
import Foreign
import Foreign.C.String
import Foreign.C.Types

data DNSResolveReply = DNSResolveReply
  { interfaceIndex :: Int
  , fullname :: String
  , hosttarget :: String
  , port :: Int
  , txt :: String
  }
  deriving (Eq, Show)

data Config = Config
  { configInterfaceIndex :: Int
  }
  deriving (Show)

defaultConfig :: Config
defaultConfig = Config{configInterfaceIndex = 0}

parseTxt :: String -> [String]
parseTxt [] = []
parseTxt (x : xs) = entry : parseTxt rest
 where
  size = ord x
  (entry, rest) = splitAt size xs

type BrowseCallback = DNSServiceRef -> CUInt -> CUInt -> CInt -> CString -> CString -> CString -> Ptr () -> IO ()

type ResolveCallback =
  DNSServiceRef -> CUInt -> CUInt -> CInt -> CString -> CString -> CUShort -> CUShort -> CString -> Ptr () -> IO ()

browseCallbackFunc :: BrowseCallback
browseCallbackFunc _ flags interfaceIndex errorCode serviceNameCString regTypeCString domainCString ctx = do
  if errorCode == c_kDNSServiceErr_NoError
    then do
      when ((flags .&. c_kDNSServiceFlagsAdd) /= 0) $ do
        resolveCallback <- wrapResolveCallback resolveCallbackFunc
        alloca $ \resolveRefPtr -> do
          err <-
            c_DNSServiceResolve
              resolveRefPtr
              0
              interfaceIndex
              serviceNameCString
              regTypeCString
              domainCString
              resolveCallback
              ctx
          if err == c_kDNSServiceErr_NoError
            then do
              resolveRef <- peek resolveRefPtr
              _ <- c_DNSServiceProcessResult resolveRef
              c_DNSServiceRefDeallocate resolveRef
            else fail (show err)
        freeHaskellFunPtr resolveCallback
    else fail $ show errorCode

resolveCallbackFunc :: ResolveCallback
resolveCallbackFunc _ _ interfaceIndex errorCode fullnameCString hosttargetCString port txtLen txtCString ctx = do
  ref <- deRefStablePtr $ castPtrToStablePtr ctx

  if errorCode == c_kDNSServiceErr_NoError
    then do
      fullname <- peekCString fullnameCString
      hosttarget <- peekCString hosttargetCString
      txt <- peekCStringLen (txtCString, fromIntegral txtLen)
      modifyIORef ref (DNSResolveReply (fromIntegral interfaceIndex) fullname hosttarget (fromIntegral $ ntohs port) txt :)
    else fail $ show errorCode

discoverHueBridges :: Config -> IO [DNSResolveReply]
discoverHueBridges config = do
  ref <- newIORef ([] :: [DNSResolveReply])
  stbPtr <- newStablePtr ref

  browseCallback <- wrapBrowseCallback browseCallbackFunc
  alloca $ \serviceRefPtr -> do
    withCString "_hue._tcp" $ \serviceType -> do
      err <-
        c_DNSServiceBrowse
          serviceRefPtr
          0
          (fromIntegral $ configInterfaceIndex config)
          serviceType
          nullPtr
          browseCallback
          (castStablePtrToPtr stbPtr)
      if err == c_kDNSServiceErr_NoError
        then do
          serviceRef <- peek serviceRefPtr
          _ <- c_DNSServiceProcessResult serviceRef
          c_DNSServiceRefDeallocate serviceRef
        else fail $ show err

  readIORef ref

foreign import capi unsafe "arpa/inet.h ntohs" ntohs :: CUShort -> CUShort

data {-# CTYPE "dns_sd.h" "struct _DNSServiceRef_t" #-} DNSServiceStruct
type DNSServiceRef = Ptr DNSServiceStruct

foreign import capi "dns_sd.h value kDNSServiceErr_NoError" c_kDNSServiceErr_NoError :: CInt
foreign import capi "dns_sd.h value kDNSServiceFlagsAdd" c_kDNSServiceFlagsAdd :: CUInt

foreign import capi "dns_sd.h DNSServiceBrowse"
  c_DNSServiceBrowse ::
    Ptr DNSServiceRef ->
    CUInt ->
    CUInt ->
    CString ->
    CString ->
    FunPtr BrowseCallback ->
    Ptr () ->
    IO CInt

foreign import capi "dns_sd.h DNSServiceProcessResult"
  c_DNSServiceProcessResult :: DNSServiceRef -> IO CInt

foreign import capi "dns_sd.h DNSServiceResolve"
  c_DNSServiceResolve ::
    Ptr DNSServiceRef ->
    CUInt ->
    CUInt ->
    CString ->
    CString ->
    CString ->
    FunPtr ResolveCallback ->
    Ptr () ->
    IO CInt

foreign import capi "dns_sd.h DNSServiceRefDeallocate"
  c_DNSServiceRefDeallocate :: DNSServiceRef -> IO ()

foreign import ccall "wrapper"
  wrapBrowseCallback :: BrowseCallback -> IO (FunPtr BrowseCallback)

foreign import ccall "wrapper"
  wrapResolveCallback :: ResolveCallback -> IO (FunPtr ResolveCallback)

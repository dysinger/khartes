module Main

%language TypeProviders

%lib Java "org.apache.jclouds:jclouds-allblobstore:1.6.3"
%lib Java "org.apache.jclouds:jclouds-blobstore:1.6.3"

%include Java "org.jclouds.*"
%include Java "org.jclouds.apis.*"
%include Java "org.jclouds.blobstore.*"
%include Java "org.jclouds.blobstore.domain.*"
%include Java "org.jclouds.providers.*"
%include Java "org.jclouds.rest.*"
%include Java "org.jclouds.s3.*"

contextBuilder : String -> IO Ptr
contextBuilder p = mkForeign (FFun "ContextBuilder.newBuilder" [FString] FPtr) p

builderCredentials : String -> String -> Ptr -> IO Ptr
builderCredentials i s b = mkForeign (FFun "credentials" [FPtr, FString, FString] FPtr) b i s

blobStoreViewContext : Ptr -> IO Ptr
blobStoreViewContext b = mkForeign (FFun "buildView(BlobStoreContext.class)" [FPtr] FPtr) b

blobStore : Ptr -> IO Ptr
blobStore c = mkForeign (FFun "getBlobStore" [FPtr] FPtr) c

closeContext : Ptr -> IO ()
closeContext c = mkForeign (FFun "close" [FPtr] FUnit) c

main : IO ()
main = do builder <- contextBuilder "aws-s3" >>= builderCredentials "KEY" "SECRET"
          context <- blobStoreViewContext builder
          store <- blobStore context
       -- do some shtuff with the blob store here
          closeContext context
          print "hello"

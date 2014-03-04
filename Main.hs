{-# LANGUAGE GADTs #-}

import           Control.Monad.IO.Class
import           Haste
import           Haste.Foreign

-- Data

data AmazonWebServices a where
  AWS :: a -> AmazonWebServices a

data ElasticComputeCloud a where
  EC2 :: a -> ElasticComputeCloud a

data SimpleStorageService a where
  S3 :: a -> SimpleStorageService a

data JavaScript where
  JS :: JavaScript
  JSRef :: Unpacked -> JavaScript

-- Classes

class AmazonWebServicesAPI a where
  aws :: MonadIO m => a -> m (AmazonWebServices a)

class AmazonWebServicesAPI a => ElasticComputeCloudAPI a where
  ec2 :: MonadIO m => AmazonWebServices a -> m (ElasticComputeCloud a)
  describeInstances :: MonadIO m => ElasticComputeCloud a -> m ()

class AmazonWebServicesAPI a => SimpleStorageServiceAPI a where
  s3 :: MonadIO m => AmazonWebServices a -> m (SimpleStorageService a)
  listBuckets :: MonadIO m => SimpleStorageService a -> m ()

-- Instances

instance AmazonWebServicesAPI JavaScript where
  aws JS = liftIO $ ffi "(function(){return require('aws-sdk');});" >>=
           return . AWS . JSRef . fromOpaque

instance ElasticComputeCloudAPI JavaScript where
  ec2 (AWS (JSRef ref)) =
    liftIO $ ffi "(function(x){return new x.EC2();});" (toOpaque ref) >>=
    return . EC2 . JSRef . fromOpaque
  describeInstances (EC2 (JSRef ref)) =
    liftIO $ ffi "(function(x){x.describeInstances().on('success',function(r){console.log(r.data);}).on('error',function(r){console.log('ERR',r.error);}).send();});" $ toOpaque ref

instance SimpleStorageServiceAPI JavaScript where
  s3 (AWS (JSRef ref)) =
    liftIO $ ffi "(function(x){return new x.S3();});" (toOpaque ref) >>=
    return . S3 . JSRef . fromOpaque
  listBuckets (S3 (JSRef ref)) =
    liftIO $ ffi "(function(x){x.listBuckets().on('success',function(r){console.log(r.data);}).on('error',function(r){console.log('ERR',r.error);}).send();});" $ toOpaque ref

-- Main

main :: IO ()
main = do
  amz <- aws JS
  ec2 amz >>= describeInstances
  s3 amz >>= listBuckets

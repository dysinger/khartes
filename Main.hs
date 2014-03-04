{-# LANGUAGE TypeFamilies #-}

import           Control.Monad.IO.Class
import           Haste
import           Haste.Foreign

class AmazonWebServicesAPI a where
  data AmazonWebServices a :: *
  aws :: MonadIO m => a -> m (AmazonWebServices a)

class AmazonWebServicesAPI a => ElasticComputeCloudAPI a where
  data ElasticComputeCloud a :: *
  ec2 :: MonadIO m => AmazonWebServices a -> m (ElasticComputeCloud a)
  describeInstances :: MonadIO m => ElasticComputeCloud a -> m ()

class AmazonWebServicesAPI a => SimpleStorageServiceAPI a where
  data SimpleStorageService a :: *
  s3 :: MonadIO m => AmazonWebServices a -> m (SimpleStorageService a)
  listBuckets :: MonadIO m => SimpleStorageService a -> m ()

data JavaScript = JS

instance AmazonWebServicesAPI JavaScript where
  data AmazonWebServices JavaScript = AmazonWebServicesJS Unpacked
  aws JS = liftIO $ ffi "(function(){return require('aws-sdk');});" >>=
           return . AmazonWebServicesJS . fromOpaque

instance ElasticComputeCloudAPI JavaScript where
  data ElasticComputeCloud JavaScript = ElasticComputeCloudJS Unpacked
  ec2 (AmazonWebServicesJS ref) =
    liftIO $ ffi "(function(x){return new x.EC2();});" (toOpaque ref) >>=
    return . ElasticComputeCloudJS . fromOpaque
  describeInstances (ElasticComputeCloudJS ref) =
    liftIO $ ffi "(function(x){x.describeInstances().on('success',function(r){console.log(r.data);}).on('error',function(r){console.log('ERR',r.error);}).send();});" $ toOpaque ref

instance SimpleStorageServiceAPI JavaScript where
  data SimpleStorageService JavaScript = SimpleStorageServiceJS Unpacked
  s3 (AmazonWebServicesJS ref) =
    liftIO $ ffi "(function(x){return new x.S3();});" (toOpaque ref) >>=
    return . SimpleStorageServiceJS . fromOpaque
  listBuckets (SimpleStorageServiceJS ref) =
    liftIO $ ffi "(function(x){x.listBuckets().on('success',function(r){console.log(r.data);}).on('error',function(r){console.log('ERR',r.error);}).send();});" $ toOpaque ref

main :: IO ()
main = do
  amz <- aws JS
  ec2 amz >>= describeInstances
  s3 amz >>= listBuckets

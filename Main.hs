{-# LANGUAGE GADTs #-}

{-

Copyright [2014] Tim Dysinger

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this file except in compliance with the License.  You
may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied.  See the License for the specific language governing
permissions and limitations under the License.

-}

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

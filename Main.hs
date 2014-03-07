{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

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

data AmazonWebServices a b where
  AWS :: a -> b -> AmazonWebServices a b

data ElasticComputeCloud a b where
  EC2 :: a -> b -> ElasticComputeCloud a b

data SimpleStorageService a b where
  S3 :: a -> b -> SimpleStorageService a b

class AmazonWebServicesAPI a b where
  aws :: MonadIO m => a -> m (AmazonWebServices a b)

class ElasticComputeCloudAPI a b where
  ec2 :: MonadIO m => AmazonWebServices a b -> m (ElasticComputeCloud a b)
  describeInstances :: MonadIO m => ElasticComputeCloud a b -> m ()

class SimpleStorageServiceAPI a b where
  s3 :: MonadIO m => AmazonWebServices a b -> m (SimpleStorageService a b)
  listBuckets :: MonadIO m => SimpleStorageService a b -> m ()

----------------
-- JAVASCRIPT --
----------------

data JavaScript where
  JS :: JavaScript

type Ptr = Unpacked

instance AmazonWebServicesAPI JavaScript Ptr where
  aws JS = liftIO $ ffi "(function(){return require('aws-sdk');});" >>=
           return . AWS JS . fromOpaque

instance ElasticComputeCloudAPI JavaScript Ptr where
  ec2 (AWS JS j) =
    liftIO $ ffi "(function(x){return new x.EC2();});" (toOpaque j) >>=
    return . EC2 JS . fromOpaque
  describeInstances (EC2 JS j) =
    liftIO $ ffi "(function(x){x.describeInstances().on('success',function(r){console.log(r.data);}).on('error',function(r){console.log('ERR',r.error);}).send();});" $ toOpaque j

instance SimpleStorageServiceAPI JavaScript Ptr where
  s3 (AWS JS j) =
    liftIO $ ffi "(function(x){return new x.S3();});" (toOpaque j) >>=
    return . S3 JS . fromOpaque
  listBuckets (S3 JS j) =
    liftIO $ ffi "(function(x){x.listBuckets().on('success',function(r){console.log(r.data);}).on('error',function(r){console.log('ERR',r.error);}).send();});" $ toOpaque j

----------
-- DEMO --
----------

main :: IO ()
main = do
  amz    <- aws JS  :: IO (AmazonWebServices JavaScript Ptr)
  amzEc2 <- ec2 amz :: IO (ElasticComputeCloud JavaScript Ptr)
  amzS3  <- s3 amz  :: IO (SimpleStorageService JavaScript Ptr)
  describeInstances amzEc2
  listBuckets amzS3

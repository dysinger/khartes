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

import           Haste
import           Haste.Foreign

data AmazonWebServices a b where
  AWS :: a -> b -> AmazonWebServices a b

data ElasticComputeCloud a b where
  EC2 :: a -> b -> ElasticComputeCloud a b

data SimpleStorageService a b where
  S3 :: a -> b -> SimpleStorageService a b

class Monad m => AmazonWebServicesAPI a b m where
  aws :: a -> m (AmazonWebServices a b)

class Monad m => ElasticComputeCloudAPI a b m where
  ec2 :: AmazonWebServices a b -> m (ElasticComputeCloud a b)
  describeInstances :: ElasticComputeCloud a b -> m ()

class Monad m => SimpleStorageServiceAPI a b m where
  s3 :: AmazonWebServices a b -> m (SimpleStorageService a b)
  listBuckets :: SimpleStorageService a b -> m ()

----------------
-- JAVASCRIPT --
----------------

data JavaScript where
  JS :: JavaScript

type Ptr = Unpacked

js :: String -> IO Ptr
js f = ffi f >>= return . fromOpaque

js1_ :: String -> Ptr -> IO ()
js1_ f = ffi f . toOpaque

js1 :: String -> Ptr -> IO Ptr
js1 f j = ffi f (toOpaque j) >>= return . fromOpaque

js2_ :: String -> Ptr -> Ptr -> IO ()
js2_ f j0 j1 = ffi f (toOpaque j0) (toOpaque j1)

js2 :: String -> Ptr -> Ptr -> IO Ptr
js2 f j0 j1 = ffi f (toOpaque j0) (toOpaque j1) >>= return . fromOpaque

instance AmazonWebServicesAPI JavaScript Ptr IO where
  aws JS = js "(function(){return require('aws-sdk');});" >>= return . AWS JS

instance ElasticComputeCloudAPI JavaScript Ptr IO where
  ec2 (AWS JS j) = js1 "(function(x){return new x.EC2();});" j >>= return . EC2 JS
  describeInstances (EC2 JS j) = js1_ "(function(x){x.describeInstances().on('success',function(r){console.log(r.data);}).on('error',function(r){console.log('ERR',r.error);}).send();});" j

instance SimpleStorageServiceAPI JavaScript Ptr IO where
  s3 (AWS JS j) = js1 "(function(x){return new x.S3();});" j >>= return . S3 JS
  listBuckets (S3 JS j) = js1_ "(function(x){x.listBuckets().on('success',function(r){console.log(r.data);}).on('error',function(r){console.log('ERR',r.error);}).send();});" j

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

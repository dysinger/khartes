module Main

{-

Copyright [2014] Tim Dysinger

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this file except in compliance with the License.  You may
obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied.  See the License for the specific language governing
permissions and limitations under the License.

-}

-- Data

data AmazonWebServices : a -> Type where
  AWS : a -> AmazonWebServices a

data ElasticComputeCloud : a -> Type where
  EC2 : a -> ElasticComputeCloud a

data SimpleDataBase : a -> Type where
  SimpleDB : a -> SimpleDataBase a

data SimpleStorageService : a -> Type where
  S3 : a -> SimpleStorageService a

data JavaScript : Type where
  JS : JavaScript
  JSRef : Ptr -> JavaScript

-- Classes

class AmazonWebServicesAPI a where
  aws : a -> IO (AmazonWebServices a)

class ElasticComputeCloudAPI a where
  ec2 : AmazonWebServices a -> IO (ElasticComputeCloud a)
  describeImages : ElasticComputeCloud a -> IO ()
  describeInstances : ElasticComputeCloud a -> IO ()

class SimpleDataBaseAPI a where
  simpledb : AmazonWebServices a -> IO (SimpleDataBase a)
  listDomains : SimpleDataBase a -> IO ()

class SimpleStorageServiceAPI a where
  s3 : AmazonWebServices a -> IO (SimpleStorageService a)
  listBuckets : SimpleStorageService a -> IO ()

-- Instances

instance AmazonWebServicesAPI JavaScript where
  aws JS = mkForeign (FFun "require('aws-sdk')" [] FPtr) >>=
           return . AWS . JSRef

instance ElasticComputeCloudAPI JavaScript where
  ec2 (AWS (JSRef p)) = mkForeign (FFun "new %0.EC2()" [FPtr] FPtr) p >>=
                        return . EC2 . JSRef
  describeImages (EC2 (JSRef p)) =
    mkForeign (
      FFun ("%0.describeImages().on('success',function(r){console.log(r.data);}).on('error',function(r){console.log('ERR',r.error);}).send()")
      [FPtr] FUnit
      ) p
  describeInstances (EC2 (JSRef p)) =
    mkForeign (
      FFun ("%0.describeInstances().on('success',function(r){console.log(r.data);}).on('error',function(r){console.log('ERR',r.error);}).send()")
      [FPtr] FUnit
      ) p

instance SimpleDataBaseAPI JavaScript where
  simpledb (AWS (JSRef p)) = mkForeign (FFun "new %0.SimpleDB()" [FPtr] FPtr) p >>=
                             return . SimpleDB . JSRef
  listDomains (SimpleDB (JSRef p)) =
    mkForeign (
      FFun ("%0.listDomains().on('success',function(r){console.log(r.data);}).on('error',function(r){console.log('ERR',r.error);}).send()")
      [FPtr] FUnit
      ) p

instance SimpleStorageServiceAPI JavaScript where
  s3 (AWS (JSRef p)) = mkForeign (FFun "new %0.S3()" [FPtr] FPtr) p >>=
                       return . S3 . JSRef
  listBuckets (S3 (JSRef p)) =
    mkForeign (
      FFun ("%0.listBuckets().on('success',function(r){console.log(r.data);}).on('error',function(r){console.log('ERR',r.error);}).send()")
      [FPtr] FUnit
      ) p

-- Main

main : IO ()
main = do amz <- aws JS
          ec2 amz >>= describeInstances
          s3 amz >>= listBuckets

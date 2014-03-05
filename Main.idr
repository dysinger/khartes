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

data AmazonWebServices : a -> b -> Type where
  AWS : a -> b -> AmazonWebServices a b

data ElasticComputeCloud : a -> b -> Type where
  EC2 : a -> b -> ElasticComputeCloud a b

data SimpleDataBase : a -> b -> Type where
  SimpleDB : a -> b -> SimpleDataBase a b

data SimpleStorageService : a -> b -> Type where
  S3 : a -> b -> SimpleStorageService a b

-- Classes

class AmazonWebServicesAPI a b where
  aws : a -> IO (AmazonWebServices a b)

class ElasticComputeCloudAPI a b where
  ec2 : AmazonWebServices a b -> IO (ElasticComputeCloud a b)
  describeImages : ElasticComputeCloud a b -> IO b
  describeInstances : ElasticComputeCloud a b -> IO b

class SimpleDataBaseAPI a b where
  simpledb : AmazonWebServices a b -> IO (SimpleDataBase a b)
  listDomains : SimpleDataBase a b -> IO b

class SimpleStorageServiceAPI a b where
  s3 : AmazonWebServices a b -> IO (SimpleStorageService a b)
  listBuckets : SimpleStorageService a b -> IO b

-- JavaScript

data JavaScript : Type where
  JS : JavaScript

data Event : Type where
  Complete : Event
  Error : Event
  Success : Event

instance Show Event where
  show Complete = "complete"
  show Error = "error"
  show Success = "success"

infixr 7 ~>
(~>) : FTy -> FTy -> FTy
(~>) a b = FFunction a b

on : Event -> (Ptr -> IO ()) -> Ptr -> IO (Ptr)
on e f j =
  mkForeign (FFun "%0.on(%1,%2)"
             [ FPtr
             , FString
             , FAny Ptr ~> FAny (IO ())
             ] FPtr) j (show e) f >>= return

send : Ptr -> IO ()
send j = mkForeign (FFun ("%0.send()") [FPtr] FUnit) j

log : Ptr -> IO ()
log j = mkForeign (FFun ("console.log(%0)") [FPtr] FUnit) j

logHandler : Ptr -> IO (Ptr)
logHandler r = on Success log r >>= on Error log >>= return

instance AmazonWebServicesAPI JavaScript Ptr where
  aws JS = mkForeign (FFun "require('aws-sdk')" [] FPtr) >>=
           return . AWS JS

instance ElasticComputeCloudAPI JavaScript Ptr where
  ec2 (AWS JS j) =
    mkForeign (FFun "new %0.EC2()" [FPtr] FPtr) j >>= return . EC2 JS
  describeImages (EC2 JS j) =
    mkForeign (FFun "%0.describeImages()" [FPtr] FPtr) j >>= return
  describeInstances (EC2 JS j) =
    mkForeign (FFun "%0.describeInstances()" [FPtr] FPtr) j >>= return

instance SimpleDataBaseAPI JavaScript Ptr where
  simpledb (AWS JS j) =
    mkForeign (FFun "new %0.SimpleDB()" [FPtr] FPtr) j >>= return . SimpleDB JS
  listDomains (SimpleDB JS j) =
    mkForeign (FFun "%0.listDomains()" [FPtr] FPtr) j >>= return

instance SimpleStorageServiceAPI JavaScript Ptr where
  s3 (AWS JS j) =
    mkForeign (FFun "new %0.S3()" [FPtr] FPtr) j >>= return . S3 JS
  listBuckets (S3 JS j) =
    mkForeign (FFun "%0.listBuckets()" [FPtr] FPtr) j >>= return

-- Main

main : IO ()
main = do
  amz <- aws JS
  amzS3 <- s3 amz
  listBuckets amzS3 >>= logHandler >>= send
  amzEc2 <- ec2 amz
  describeInstances amzEc2 >>= logHandler >>= send

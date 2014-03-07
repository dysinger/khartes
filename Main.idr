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

-- DATA

data AmazonWebServices : a -> b -> Type where
  AWS : a -> b -> AmazonWebServices a b

data ElasticComputeCloud : a -> b -> Type where
  EC2 : a -> b -> ElasticComputeCloud a b

data SimpleDataBase : a -> b -> Type where
  SimpleDB : a -> b -> SimpleDataBase a b

data SimpleStorageService : a -> b -> Type where
  S3 : a -> b -> SimpleStorageService a b

-- CLASSES

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

-- JAVASCRIPT

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

-- GENERAL JS

infixr 7 ~>
(~>) : FTy -> FTy -> FTy
(~>) a b = FFunction a b

jsMap : (Ptr -> IO (Ptr)) -> Ptr -> IO (Ptr)
jsMap j0 j1 =
  mkForeign (FFun ("%0.map(%1)")
             [FPtr, FAny Ptr ~> FAny (IO (Ptr))] FPtr) j1 j0

forEach : (Ptr -> IO ()) -> Ptr -> IO ()
forEach j0 j1 =
  mkForeign (FFun ("%0.forEach(%1)")
             [FPtr, FAny Ptr ~> FAny (IO ())] FUnit) j1 j0

-- AWS CONFIG

region : AmazonWebServices JavaScript Ptr -> String -> IO ()
region (AWS JS j0) j1 =
  mkForeign (FFun ("%0.config.update({region: %1})") [FPtr, FString] FUnit) j0 j1

-- AWS REQUEST

on : Event -> (Ptr -> IO ()) -> Ptr -> IO (Ptr)
on e f j =
  mkForeign (FFun "%0.on(%1,%2)"
             [ FPtr
             , FString
             , FAny Ptr ~> FAny (IO ())
             ] FPtr) j (show e) f

send : Ptr -> IO ()
send j = mkForeign (FFun ("%0.send()") [FPtr] FUnit) j

-- AWS RESPONSE

rsErr : Ptr -> IO (Ptr)
rsErr j = mkForeign (FFun ("%0.error") [FPtr] FPtr) j

rsData : Ptr -> IO (Ptr)
rsData j = mkForeign (FFun ("%0.data") [FPtr] FPtr) j

-- ATTRIBUTES

name : Ptr -> IO (Ptr)
name j = mkForeign (FFun ("%0.Name") [FPtr] FPtr) j

buckets : Ptr -> IO (Ptr)
buckets j = mkForeign (FFun ("%0.Buckets") [FPtr] FPtr) j

bucketNames : Ptr -> IO (Ptr)
bucketNames j = rsData j >>= buckets >>= jsMap name

reservationId : Ptr -> IO (Ptr)
reservationId j = mkForeign (FFun ("%0.ReservationId") [FPtr] FPtr) j

reservations : Ptr -> IO (Ptr)
reservations j = mkForeign (FFun ("%0.Reservations") [FPtr] FPtr) j

reservationIds : Ptr -> IO (Ptr)
reservationIds j = rsData j >>= reservations >>= jsMap reservationId

instances : Ptr -> IO (Ptr)
instances j = mkForeign (FFun ("%0.Instances") [FPtr] FPtr) j

instanceId : Ptr -> IO (Ptr)
instanceId j = mkForeign (FFun ("%0.InstanceId") [FPtr] FPtr) j

instanceIds : Ptr -> IO (Ptr)
instanceIds j =
  rsData j >>= reservations >>= jsMap instances >>= jsMap (jsMap instanceId)

-- LOGGING

log : Ptr -> IO ()
log j = mkForeign (FFun ("console.log(%0)") [FPtr] FUnit) j

logErr : Ptr -> IO ()
logErr j = rsErr j >>= log

logData : Ptr -> IO ()
logData j = rsData j >>= log

logEachBucketName : Ptr -> IO ()
logEachBucketName j = do putStrLn "BUCKET NAMES:"
                         bucketNames j >>= forEach log

logEachReservationId : Ptr -> IO ()
logEachReservationId j = do putStrLn "RESERVATIONS:"
                            reservationIds j >>= forEach log

logEachInstance : Ptr -> IO ()
logEachInstance j = do putStrLn "INSTANCES:"
                       instanceIds j >>= forEach log

-- INSTANCES

instance AmazonWebServicesAPI JavaScript Ptr where
  aws JS = mkForeign (FFun "require('aws-sdk')" [] FPtr) >>=
           return . AWS JS

instance ElasticComputeCloudAPI JavaScript Ptr where
  ec2 (AWS JS j) =
    mkForeign (FFun "new %0.EC2()" [FPtr] FPtr) j >>= return . EC2 JS
  describeImages (EC2 JS j) =
    mkForeign (FFun "%0.describeImages()" [FPtr] FPtr) j
  describeInstances (EC2 JS j) =
    mkForeign (FFun "%0.describeInstances()" [FPtr] FPtr) j

instance SimpleDataBaseAPI JavaScript Ptr where
  simpledb (AWS JS j) =
    mkForeign (FFun "new %0.SimpleDB()" [FPtr] FPtr) j >>= return . SimpleDB JS
  listDomains (SimpleDB JS j) =
    mkForeign (FFun "%0.listDomains()" [FPtr] FPtr) j

instance SimpleStorageServiceAPI JavaScript Ptr where
  s3 (AWS JS j) =
    mkForeign (FFun "new %0.S3()" [FPtr] FPtr) j >>= return . S3 JS
  listBuckets (S3 JS j) =
    mkForeign (FFun "%0.listBuckets()" [FPtr] FPtr) j

-- MAIN

main : IO ()
main = do
  amz <- aws JS
  region amz "us-east-1"
  ec2 amz >>=
    describeInstances >>=
    on Success logEachInstance >>=
    on Error logErr >>=
    send
  s3 amz >>=
    listBuckets >>=
    on Success logEachBucketName >>=
    on Error logErr >>=
    send

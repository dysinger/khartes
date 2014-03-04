module Main

data AmazonWebServices : a -> Type where
  AWS : a -> AmazonWebServices a

data ElasticComputeCloud : a -> Type where
  EC2 : a -> ElasticComputeCloud a

data SimpleDataBase : a -> Type where
  SimpleDB : a -> SimpleDataBase a

data SimpleStorageService : a -> Type where
  S3 : a -> SimpleStorageService a

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

data JavaScript : Type where
  JS : JavaScript
  JSPtr : Ptr -> JavaScript

instance AmazonWebServicesAPI JavaScript where
  aws JS = mkForeign (FFun "require('aws-sdk')" [] FPtr) >>=
           return . AWS . JSPtr

instance ElasticComputeCloudAPI JavaScript where
  ec2 (AWS (JSPtr p)) = mkForeign (FFun "new %0.EC2();" [FPtr] FPtr) p >>=
                        return . EC2 . JSPtr
  describeImages (EC2 (JSPtr p)) =
    mkForeign (
      FFun ("%0.describeImages().on('success',function(r){console.log(r.data);}).on('error',function(r){console.log('ERR',r.error);}).send()")
      [FPtr] FUnit
      ) p
  describeInstances (EC2 (JSPtr p)) =
    mkForeign (
      FFun ("%0.describeInstances().on('success',function(r){console.log(r.data);}).on('error',function(r){console.log('ERR',r.error);}).send()")
      [FPtr] FUnit
      ) p

instance SimpleDataBaseAPI JavaScript where
  simpledb (AWS (JSPtr p)) = mkForeign (FFun "new %0.SimpleDB()" [FPtr] FPtr) p >>=
                             return . SimpleDB . JSPtr
  listDomains (SimpleDB (JSPtr p)) =
    mkForeign (
      FFun ("%0.listDomains().on('success',function(r){console.log(r.data);}).on('error',function(r){console.log('ERR',r.error);}).send()")
      [FPtr] FUnit
      ) p

instance SimpleStorageServiceAPI JavaScript where
  s3 (AWS (JSPtr p)) = mkForeign (FFun "new %0.S3()" [FPtr] FPtr) p >>=
                       return . S3 . JSPtr
  listBuckets (S3 (JSPtr p)) =
    mkForeign (
      FFun ("%0.listBuckets().on('success',function(r){console.log(r.data);}).on('error',function(r){console.log('ERR',r.error);}).send()")
      [FPtr] FUnit
      ) p

main : IO ()
main = do amz <- aws JS
          ec2 amz >>= describeInstances
          s3 amz >>= listBuckets

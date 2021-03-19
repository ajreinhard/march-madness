cd "C:\Users\Owner\Documents\GitHub\march-madness"
"C:\Program Files\R\R-4.0.0\bin\Rscript.exe" live-scoring.R
cd madness
aws s3 cp . s3://statbutler.com/madness/ --recursive
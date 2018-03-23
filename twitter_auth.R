rm(list = ls())
install.packages("base64enc")
install.packages("RCurl")
install.packages("ROAuth")
install.packages("twitteR")
library("base64enc")
library("twitteR")
library("ROAuth")
library("RCurl")

reqURL <- "http://api.twitter.com/oauth/request_token"
accessURL <- "http://api.twitter.com/oauth/access_token"
authURP <- "http://api.twitter.com/oauth/authorize"

consumerKey <- "0hj2HIxjNZfVh5KxU84Sm6cp8"
consumerSecert <- "AolyqCnyMm8pXCaGpBXVzxW6cY77OxQWi23ym4QxAGvT3ttPdF"
accesstoken <- "518299811-pfAvgAV2tWGmQ1eEZyAcZ1SScS0DlRDXVUTQimFl"
accesstokensecret <- "r7hri2qlI3evKL5LS59k7zVTDxvbtBAV9sNFwLxg2iItV"

option(RCurlOptions = list(cainfo =system.file("CurlssL", "cacert.pem", package = "RCurl")))
download.file(url = "http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")

setup_twitter_oauth(consumerKey, consumerSecert, accesstoken, accesstokensecret)

myTweet <- searchTwitter("@근로시간", n=1000)


padLeftWithZeros <-
function (s, len) 
{
    paste(rep("0", len - nchar(s)), s, sep = "")
}

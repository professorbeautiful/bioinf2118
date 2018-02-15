
padLeftWithZeros <- function (s, len) 
  paste(rep("0", len - nchar(s)), s, sep = "")


addOpacity = function(color, opacity="88")  {
  if(length(color) > 1)
    return(sapply(color, addOpacity, opacity=opacity))
  if(substring(color,1,1) != "#")
    color=	paste(	"#", sep="", paste(unlist(
      lapply(
        lapply(col2rgb(color), format.hexmode),
        padLeftWithZeros, len=2)
    ),
    collapse=""))
  paste(color,opacity,sep="")
}

gifts <- c("A partridge in a pear tree.", "Two turtle doves and", "Three french hens", "Four calling birds", "Five golden rings", "Six geese a-laying", "Seven swans a-swimming", "Eight maids a-milking", "Nine ladies dancing", "Ten lords a-leaping", "Eleven pipers piping", "Twelve drummers drumming")
days <- c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth")
for(i in 1:12) {
  cat(paste("On the",days[i],"day of Christmas","\n","my true love gave to me:","\n"),paste(gifts[seq(i,1)],"\n"))
}

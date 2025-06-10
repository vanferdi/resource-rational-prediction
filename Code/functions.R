##############################################################################
# define some general functions that can be used across the different R files

# some of the examples below use the data frame called df, 
# load it via load_data.R to run the examples

##############################################################################
# convert_to_array()
# convert a character string to an indexable array

# input: a character string of states/symbols with no separator, ex: "010110100101"
# output: an indexable array of states/symbols stored as numbers
convert_to_array <- function(string) { 
    return(as.numeric(strsplit(string,"")[[1]]))
}

# example usage:
# convert_to_array("010110100101")

##############################################################################
# convert_to_array2()
# convert a character string with commas to an indexable array

# input: a character string of states/symbols separated by commas, ex: "0,1,0,1,1"
# output: an indexable array of states/symbols stored as numbers
convert_to_array2 <- function(string) {
    return(as.numeric(strsplit(string,",")[[1]]))
}

# example usage:
# convert_to_array2("0,1,0,1,1")

##############################################################################
# get_data()
# for any row in df that you specify,
# get all of the string-format data that needs to be parsed from strings to arrays
# and return it all in array format

# input:  1) df 
#         2) the row you want to extract data from
# output: a list of parsed data in the following order:
#         1) experimental condition
#         2) hidden sequence
#         3) input sequence
#         4) output sequence
#         5) accuracy per trial
#         6) response interval

get_data <- function(df,row) {
    d <- df[row,]
    typ <- as.character(d$input_type)
    inp <- convert_to_array(d$input_sequence)
    out <- convert_to_array(d$output_sequence)
    hid <- convert_to_array(d$hidden_sequence)
    acc <- convert_to_array(d$accuracy)
    rts <- convert_to_array2(d$RT)
    return(list(typ,hid,inp,out,acc,rts))
}

# example usage:
# get_data(df,1)[[4]]     # get the "output sequence" responses from the first session
# get_data(df,2)[[6]][1]  # get first RT from second session  # 1545

##############################################################################
# myprint()
# prints an array to screen as a string without quotes

myprint <- function(array_of_printables) {
    print(noquote(paste(array_of_printables,collapse=" ")))
}
# example usage:
# the <- "the"
# myprint(c("here's",the,"thing"))

##############################################################################
# END
##############################################################################
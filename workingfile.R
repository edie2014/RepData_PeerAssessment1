completeactivity_copy$int_nchar <- sapply(completeactivity_copy$interval, nchar)

for (i in 1:nrow(completeactivity_copy)) {
    if (completeactivity_copy[i, 'int_nchar'] == 1) {
        completeactivity_copy[i, 'interval'] <- paste0("00:0",
                                                      completeactivity_copy[i, 'interval'])
    } else if (completeactivity_copy[i, 'int_nchar'] == 2) {
        completeactivity_copy[i, 'interval'] <- paste0("00:",
                                                       completeactivity_copy[i, 'interval'])
    } else if (completeactivity_copy[i, 'int_nchar'] == 3) {
        completeactivity_copy[i, 'interval'] <- paste0("0",
                                                       substr(completeactivity_copy[i, 'interval'], 1, 1),
                                                       ":",
                                                       substr(completeactivity_copy[i, 'interval'], 2, 3))
    } else {
        completeactivity_copy[i, 'interval'] <- paste0(substr(completeactivity_copy[i, 'interval'], 1, 2),
                                                       ":",
                                                       substr(completeactivity_copy[i, 'interval'], 3, 4))
    }
}

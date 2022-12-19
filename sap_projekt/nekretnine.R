real_astate = read.csv("preprocessed_data.csv")
real_astate$YearBuilt = as.integer(real_astate$YearBuilt)

real_astate.grouped = aggregate(real_astate[c("GrLivArea","SalePrice")],
                                real_astate[c("BedroomAbvGr")],
                                FUN = sum)

print(real_astate.grouped)

barplot(real_astate.grouped$SalePrice/real_astate.grouped$GrLivArea,
        main = "broj soba",
        ylab = "cijena po kvadratu",
        names.arg = real_astate.grouped$BedroomAbvGr,
        las=2)
  # simulate cutoff of the subtree starting from the outlook=sunny split
rptree.stc <- rpart(play~., weather, subset=!(outlook %in% c("rainy", "sunny") &
                                              humidity=="high" & outlook!="sunny"),
                    minsplit=2, cp=0)

prp(rptree, varlen=0, faclen=0, main="Before subtree cutoff")
prp(rptree.stc, varlen=0, faclen=0,  main="After subtree cutoff")

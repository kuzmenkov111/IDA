weathercl.std <- predict.std(std.all(.~., weathercl), weathercl)
dissmat(weathercl.std, euc.dist)

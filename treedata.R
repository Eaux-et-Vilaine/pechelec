# TODO: Add comment
# 
# Author: cedric.briand
###############################################################################


account_id <- c(1:11)
account_name <- c('root_account','dining', 'food', 'discretionary_expense',
    'expenses', 'base_salary_wife', 'base_salary_husband',
    'base_salary', 'salary', 'taxable_income',
    'income')
account_parentid <- c(NA,3,4,5,1,8,8,9,10,11,1)
test.data <- data.frame(account_id, account_parentid, account_name, stringsAsFactors = F)
library(data.tree)
tree1 <- FromDataFrameNetwork(test.data[-1,])
tree1$account_name <- 'root_account'
ToDataFrameTree(tree1, account = 'name', 'account_name', 'pathString')


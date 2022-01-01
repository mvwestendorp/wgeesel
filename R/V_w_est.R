V_w_est <-
function(id,U,logit_S,sumUS,D,W,V){
  mat=lapply(1:length(unique(id)),function(m_v){
    E_i_est(m_v,U,logit_S,sumUS,D,W,V)
  })
 x_mat_1=Reduce("+", lapply(mat, function(x) replace(x[[1]], is.na(x[[1]]), 0)))
 x_mat_2=Reduce("+", lapply(mat, function(x) replace(x[[2]], is.na(x[[2]]), 0)))
 V_w=solve(x_mat_2)%*%x_mat_1%*%solve(x_mat_2)
 return(V_w)
}

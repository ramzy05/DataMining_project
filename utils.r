
pretaitement <- function (df){
  # Rénommons PAY_0 en PAY_1,comme ça on ne quitte plus de PAY_0 subitement à PAY_2
  df[1,][df[1,]=='PAY_0'] = 'PAY_1'
  return (df)
}
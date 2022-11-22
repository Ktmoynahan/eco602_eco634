n <- 17
vec_1 = sample(10, n, replace = TRUE)
for (i in 1:n)
{
  print(
    paste0("The element of vec_1 at index", i, "is", vec_1[i]))
}
create_and_print_vec = function(n, min = 1 , max = 10)
{
    vec_2 = sample(min:max, n, replace = TRUE)
    for (i in 1:n)
    {
  print(paste0("The element at index", i, "is", vec_2[i]))
    }
}
create_and_print_vec(5)
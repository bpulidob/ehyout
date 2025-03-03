# Area-based epigraph index
ABEI <- function(curves){
  index <- numeric()
  n_curves <- dim(curves)[1]
  lengthcurves <- dim(curves)[2]
  t_curves <- t(curves) # transpose so we can compute diffs all at once
  for(i in 1:n_curves){
    index[i] <- 0
    diffs <- t_curves - t_curves[,i]
    diffs <- diffs * (diffs > 0)
    index[i] <- diffs |>  rowMeans() |> sum() #vectorized ops for speed
  }
  return(index)
}

# Area-based hypograph index
ABHI <- function(curves){
  return(ABEI(-curves))
}

# Calculate area-based indices on data, first and second derivatives
ind_all <- function(dataset){
  library(tf)
  
  if(!is.matrix(dataset)){
    dataset <- as.matrix(dataset)
  }
  
  sm_tf <- tfd(data = dataset, evaluator = tf_approx_spline)
  sm_tf <- tibble::tibble(dat = sm_tf,
                          deriv = tf_derive(sm_tf), # first derivatives
                          deriv2 = tf_derive(sm_tf, order=2)) # second derivatives
  
  sm_tf <- tibble::tibble(ABEI=sm_tf$dat |> as.matrix() |> ABEI(),
                          ABHI=sm_tf$dat |> as.matrix() |> ABHI(),
                          ABEI_d=sm_tf$deriv |> as.matrix() |> ABEI(),
                          ABHI_d=sm_tf$deriv |> as.matrix() |> ABHI(),
                          ABEI_d2=sm_tf$deriv2 |> as.matrix() |> ABEI(),
                          ABHI_d2=sm_tf$deriv2 |> as.matrix() |> ABHI(),
                          MEI=sm_tf$dat |> as.matrix() |> MEI(),
                          MHI=sm_tf$dat |> as.matrix() |> MHI(),
                          MEI_d=sm_tf$deriv |> as.matrix() |> MEI(),
                          MHI_d=sm_tf$deriv |> as.matrix() |> MHI(),
                          MEI_d2=sm_tf$deriv2 |> as.matrix() |> MEI(),
                          MHI_d2=sm_tf$deriv2 |> as.matrix() |> MHI(),
                          EI=sm_tf$dat |> as.matrix() |> EI(),
                          HI=sm_tf$dat |> as.matrix() |> HI(),
                          EI_d=sm_tf$deriv |> as.matrix() |> EI(),
                          HI_d=sm_tf$deriv |> as.matrix() |> HI(),
                          EI_d2=sm_tf$deriv2 |> as.matrix() |> EI(),
                          HI_d2=sm_tf$deriv2 |> as.matrix() |> HI()
  )
  
  return(sm_tf)
}

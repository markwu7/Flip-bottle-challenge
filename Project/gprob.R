gbinom = function(n,p,scale=FALSE,
                  a=ifelse(scale,floor(n*p-4*sqrt(n*p*(1-p))),0),
                  b=ifelse(scale,ceiling(n*p+4*sqrt(n*p*(1-p))),n),
                  main=NULL,...) {
  # load the ggplot2
  require(ggplot2)
  # make sure a and b are integers
  a = round(a)
  b = round(b)
  # make sure a < b
  if(a > b) {
    temp = a
    a = b
    b = temp
  }
  # make sure a and b are in range
  if(a < 0)
    a = 0
  if(b > n)
    b = n
  # create the sequence of possible values to graph
  x = seq(a,b)
  # compute the probabilities to graph
  probability = dbinom(x,n,p)
  # Choose a title for the plot if one is not passed
  if(is.null(main))
    main = paste("Binomial(",n,",",p,")")
  
  # save the graph as an object which can be returned
  df = data.frame(x,probability)
  graph = ggplot(df,aes(x=x,y=probability,xend=x,yend=0),...) +
    geom_segment(...) +
    xlab('Times') +
    ylab('Probability') +
    geom_hline(yintercept=0) +
    ggtitle(main)
  plot(graph)
  # return the graph object, but do so invisibly so no output is shown on the screen
  return(invisible(graph))
}

geom_norm_density = function(mu=0,sigma=1,a=NULL,b=NULL,color="blue",...)
{
  if ( is.null(a) )
  {
    a = qnorm(0.0001,mu,sigma)
  }
  if ( is.null(b) )
  {
    b = qnorm(0.9999,mu,sigma)
  }
  x = seq(a,b,length.out=1001)
  df = data.frame(
    x=x,
    y=dnorm(x,mu,sigma)
  )
  geom_line(data=df,aes(x=x,y=y),color=color,...)
}

geom_norm_fill = function(mu=0,sigma=1,a=NULL,b=NULL,
                          fill="firebrick4",...)
{
  if ( is.null(a) )
  {
    a = qnorm(0.0001,mu,sigma)
  }
  if ( is.null(b) )
  {
    b = qnorm(0.9999,mu,sigma)
  }
  x = seq(a,b,length.out=1001)
  df = data.frame(
    x=x,
    ymin=rep(0,length(x)),
    ymax = dnorm(x,mu,sigma)
  )
  geom_ribbon(data=df,aes(x=x,ymin=ymin,ymax=ymax,y=NULL),fill=fill,...)
}

gnorm = function(mu=0,sigma=1,a=NULL,b=NULL,color="blue",
                 fill=NULL,title=TRUE,...)
{
  g = ggplot()
  
  if ( !is.null(fill) )
    g = g + geom_norm_fill(mu,sigma,a,b,fill)
  
  g = g +
    geom_norm_density(mu,sigma,a,b,color,...) +
    geom_hline(yintercept=0) +
    ylab('density')
  
  if ( title )
    g = g +
    ggtitle(paste("N(",mu,",",sigma,")"))
  return ( g )
}

geom_chisq_null_a = function(df)
{
  if ( df < 2 )
    a = qchisq(0.05,df)
  else
    a = qchisq(0.0001,df)
  return ( a )
}

geom_chisq_null_b = function(df)
{
  if ( df < 2 )
    b = qchisq(0.95,df)
  else
    b = qchisq(0.9999,df)
  return ( b )
}

geom_chisq_density = function(df=1,a=NULL,b=NULL,color="blue",...)
{
  if ( is.null(a) )
    a = geom_chisq_null_a(df)
  if ( is.null(b) )
    b = geom_chisq_null_b(df)
  x = seq(a,b,length.out=1001)
  dat = data.frame(
    x=x,
    y=dchisq(x,df)
  )
  geom_line(data=dat,aes(x=x,y=y),color=color,...)
}

geom_chisq_fill = function(df=1,a=NULL,b=NULL,
                           fill="firebrick4",...)
{
  if ( is.null(a) )
    a = geom_chisq_null_a(df)
  if ( is.null(b) )
    b = geom_chisq_null_b(df)
  x = seq(a,b,length.out=1001)
  dat = data.frame(
    x=x,
    ymin=rep(0,length(x)),
    ymax = dchisq(x,df)
  )
  geom_ribbon(data=dat,aes(x=x,ymin=ymin,ymax=ymax,y=NULL),fill=fill,...)
}

gchisq = function(df=1,a=NULL,b=NULL,color="blue",
                  fill=NULL,title=TRUE,...)
{
  g = ggplot()
  
  if ( !is.null(fill) )
    g = g + geom_chisq_fill(df,a,b,fill)
  
  g = g +
    geom_chisq_density(df,a,b,color,...) +
    geom_hline(yintercept=0) +
    ylab('density')
  
  if ( title )
    g = g +
    ggtitle(paste("Chi-square(",df,")"))
  return ( g )
}



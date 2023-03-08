######
# file from MINMA package
######

expDatFilter=function(edat,missUpperRate=0.2){
  #   Remove features with over missUpperRate 0s => too many missing observations should be removed.
  #
  #   Args:
  #     edat: matrix (#features * #patients), expression data matrix.
  #     missUpperRate: (0,1), features with missing rate > missUpperRate will be removed.
  #
  #   Returns:
  #     A matrix of (#features * #patients).
  sampRate=apply(edat,1,function(x){sum(x==0)})/ncol(edat)
  ll=which(sampRate<missUpperRate)
  res=edat[ll,]
  rownames(res)=ll
  return(res)
}

F_Map_M=function(edat,matchdat,cgmat,maxconsider=20){
  ref=colnames(cgmat)
  res=t(sapply(rownames(edat),function(loc){
    l=which(loc==matchdat[,4])
    if(length(l)==0){
      rep(NA,maxconsider)
    }else{
      nn=matchdat[l,1]
      ll=match(nn,ref)
      ll[1:maxconsider]
    }}))
  ncg=apply(res,1,function(x){return(sum(!is.na(x)))})
  #hist(ncg,col="blue",main="Features maps to #CG")
  res=res[,1:max(ncg),drop=F]
  rownames(res)=rownames(edat)
  return(res)
}

ionMass=function(adduct.dataframe,refions){
  #   Calculate ion mass pairwise comparison matrix.
  #
  #   Args:
  #     adduct.dataframe: adduct table
  #     refions: ions considered, if not given, then consider every ions...
  #   Returns:
  #     A vector: addition measure of considered ions.


  posLoc=substr(adduct.dataframe$charge,start=1,stop=2)
  posLoc=which(posLoc=="1+")
  adduct=as.character(adduct.dataframe$adduct[posLoc])
  nmax=sapply(adduct,nchar)
  nmax=max(nmax)
  y=sapply(adduct,function(x){
    if(nchar(x)<nmax){
      paste0(x,paste0(rep(" ",(nmax-nchar(x))),sep="",collapse = ""),sep="",collapse = "")
    }
  })
  if(methods::hasArg("refions")){
    ionsLoc=sapply(refions,function(x){
      n=nchar(x)
      xx=paste0("M+",x,sep="",collapse = "")
      which(y==paste0(xx,paste0(rep(" ",(nmax-nchar(xx))),sep="",collapse = ""),sep="",collapse = ""))
    })
    ionsLoc=posLoc[ionsLoc]
  }else{
    ionsLoc=posLoc
  }
  return(adduct.dataframe$addition[ionsLoc])
}

mzMat=function(edat,mzdat,refv,cutoff=1e-5,tcutoff=100){

  loc=as.numeric(rownames(edat))
  nn=mzdat[loc,"mz"]
  tt=mzdat[loc,"time"]

  td=as.matrix(stats::dist(tt))
  matd=as.matrix(stats::dist(nn))
  refd=stats::dist(refv)
  rrd=as.matrix(refd)
  res=matrix(0,ncol=length(loc),nrow=length(loc))

  i=1
  j=0
  for(k in 1:length(refd)){
    j=j+1
    if(j>(length(refv)-i)){
      i=i+1
      j=1
    }
    # cat("ij",i,j,"\n")
    a=abs(matd-refd[k])
    aa=which(a<refv[i]*cutoff)
    bb=which(td<tcutoff)
    ll=intersect(aa,bb)
    #rres[aa]=1
    res[ll]=1
  }
  return(res)
}

FFMat=function(edat,ch2cgmat,matchdat,neilev=1,cgmat){
  tmp=cgmat
  ss=cgmat
  while(neilev>1){
    tmp=tmp*cgmat
    ss=ss+tmp
  }
  ss[which(ss>0)]=1

  cad=apply(ch2cgmat,1,function(x){return(sum(!is.na(x)))})
  cad=which(cad>0)
  res=matrix(0,ncol=nrow(ch2cgmat),nrow=nrow(ch2cgmat))
  cgnames=rownames(cgmat)
  chloc=rownames(edat)
  loc=as.numeric(chloc)
  for(h in cad){
    t=ch2cgmat[h,]
    t=t[!is.na(t)]
    ncgset=sapply(1:length(t),function(x){return(which(ss[t,]>0))})
    ncg=unique(unlist(ncgset))
    ncg=ncg[ncg<nrow(matchdat)]
    if(length(ncg)>0){
      nch=intersect(as.numeric(matchdat[ncg,4]),loc)
      if(length(nch)==0){
      }else{
        nch=sapply(1:length(nch),function(s){which(loc==nch[s])})
        res[h,nch]=1
      }
    }
  }
  # res=res+t(res)
  # res[res>0]=1
  rownames(res)=rownames(ch2cgmat)
  colnames(res)=rownames(res)
  return(list(MMMat=ss,FFMat=res))
}

mis.pattern=function(def=c("percent.feature","percent.sb"),dat,index.ch,ch2chmat){
  def=match.arg(def)
  tsb=ncol(dat)
  res.nch=matrix(nrow=nrow(dat),ncol=nrow(dat)) ##change to obs.
  rownames(res.nch)=rownames(dat)
  colnames(res.nch)=rownames(dat)
  res.ch=apply(dat,1,function(x){return(sum(!is.na(x)))})/tsb
  cad=which(index.ch>0)
  cc=sapply(cad,function(s){length(which(ch2chmat[s,]>0))})

  for(i in cad){
    sbmis=which(is.na(dat[i,]))
    #    res.ch[i]=1-length(sbmis)/tsb
    nch=ch2chmat[i,]
    nch=which(nch>0)
    cc=dat[nch,sbmis,drop=F]

    if(sum(is.na(cc))>0){
      kk=which(is.na(cc),arr.ind=T)
      kk=as.matrix(kk)
      if(def=="percent.feature"){
        res.nch[i,nch]=1
        res.nch[i,nch[unique(kk[,1])]]=0
      }
      if(def=="percent.sb"){
        dd=apply(cc,1,function(x){sum(!is.na(x))})
        res.nch[i,nch]=dd/length(sbmis)
        # cat("i",i," nch",nch,"\n")
      }
    }else{
      res.nch[i,nch]=1
    }
    #    test=c(test,sum(is.na(res.nch[i,nch])))
  }
  # nss=rowSums(ch2chmat)
  return(list(misdef=def,obs.pattern.nch=res.nch,obs.score.ch=res.ch))
}

knnMat=function(dat,method=c("euclidean","pearson.corr")){
  m=match.arg(method)
  if(m=="euclidean"){
    d=stats::dist(dat,m)
    d=as.matrix(d)
    diag(d)=Inf
  }else{
    d=Hmisc::rcorr(t(dat),type="pearson")
    d=d$r
    d=1-abs(d)
    diag(d)=Inf
  }
  return(d)
}

rd=function(x,y){
  sel<-which(!is.na(x) & !is.na(y))
  return(sum(abs(diff(x[sel][order(y[sel])])))/length(sel))
}

my.diff=function(x){
  sel<-!is.na(x)
  return(sum(abs(diff(x[sel])))/(sum(sel)-1))
}

rd.matrix.order=function(a,x) {
  a<-a[,which(!is.na(x))]
  x<-x[which(!is.na(x))]
  a<-a[,order(x)]
  #	dd<-apply(a,1,my.diff)
  d<-a[,2:ncol(a)]-a[,1:(ncol(a)-1)]
  dd<-apply(abs(d),1,sum,na.rm=T)/apply(!is.na(d),1,sum)
  return(dd)
}

rd.matrix=function(a, direction=1){
  rdmat<-matrix(0,nrow(a),nrow(a))
  for(i in 1:nrow(a))
  {
    rdmat[i,]<-rd.matrix.order(a,a[i,])  ### rows.of.a | a[i,]
  }
  if(direction == 2)
  {
    rdmat.diff<-rdmat-t(rdmat)
    sel<-which(rdmat.diff>0)
    rdmat[sel]<-t(rdmat)[sel]
  }else{
    rdmat<-t(rdmat)
  }
  return(rdmat)
}

networkInfor=function(edat,ffmat,mzmzmat,bcov.num=5,dcol.num=5,knn.num=5){

  nbr.net=ffmat+mzmzmat # 0/1
  nfeature=nrow(nbr.net)
  diag(nbr.net)=0
  knnmat=knnMat(edat,method="euclidean") #two similar, the correlation smaller.
  nbr.knn=t(apply(knnmat,1,function(x){
    xx=sort.int(x,decreasing = F,index.return = T)
    res=rep(0,nfeature)
    res[xx$ix[1:knn.num]]=1
    res
  }))
  dmat=Hmisc::rcorr(t(edat),type="pearson")$r
  dcol=rd.matrix(edat)
  diag(dcol)=999999
  nbr.dcol=t(apply(dcol,1,function(x){
    xx=sort.int(x,decreasing = F,index.return = T)
    res=rep(0,nfeature)
    res[xx$ix[1:dcol.num]]=1
    res
  }))
  bcov=Hmisc::rcorr(t(dmat),type="pearson")$r
  bcov=abs(bcov)
  diag(bcov)=0
  nbr.bcov=t(apply(bcov,1,function(x){
    xx=sort.int(x,decreasing = F,index.return = T)
    res=rep(0,nfeature)
    res[xx$ix[1:bcov.num]]=1
    res
  }))

  nbrmat=nbr.net+nbr.knn+nbr.bcov+nbr.dcol ## direct graph NOW. only row infor used.
  nbrmat[nbrmat>0]=1
  diag(nbrmat)=0

  mis=apply(edat,1,function(x){sum(is.na(x))})/ncol(edat)
  avr.nbr.mis=sapply(1:nfeature,function(n){
    mean(mis[which(nbrmat[n,]>0)])
  })
  ref=which(apply(edat,1,function(x){sum(is.na(x))>0}))
  ref=ref[sort.int(avr.nbr.mis[ref],decreasing = F,index.return = T)$ix]
  names(ref)
  return(list(nbrMat=nbrmat,impSeqRefVec=ref,weights=abs(dmat)))
}

impute.SVR=function(sim,nbrmat,impseq,k,rowmax=0.5,colmax=0.8, maxp=1000){
  for(loc in impseq){
      # print(loc)
      y=sim[loc,]
      mis=which(is.na(y))
      nbrs=which(nbrmat[loc,]>0)
      mis.obs.row=nbrs[which(!is.na(rowSums(sim[nbrs,mis,drop=F])))]
      if(length(mis.obs.row)>0){
        full.obs.col=which(!is.na(colSums(sim[mis.obs.row,-mis,drop=F])))
        if(length(full.obs.col)>1){
          loc.col=seq(1,ncol(sim))[-mis][full.obs.col]
          dat.train=lapply(mis.obs.row,function(r){
            sim[r,loc.col]
          })
          dat.train$y=y[loc.col]
          dat.train=as.data.frame(dat.train)

          dat.test=lapply(mis.obs.row,function(r){
            sim[r,mis]
          })
          dat.test=as.data.frame(dat.test)
          # attach(dat.train)
          model.fit=e1071::svm(y~., data=dat.train)
          # attach(dat.test)
          pp=stats::predict(model.fit,newdata=dat.test)
          sim[loc,mis]=pp
        }
      }
  }

  if(sum(is.na(sim))>0){
    sim=impute::impute.knn(sim,k,rowmax,colmax,maxp)$data
  }
  return(sim)
}

impute.KNN=function(sim,nbrmat,impseq,k,rowmax=0.5,colmax=0.8, maxp=1000){
  for(loc in impseq){
    myknn.mat=sim[c(loc,which(nbrmat[loc,]>0)),]
    myknn.imp=tryCatch({impute::impute.knn(myknn.mat,k,rowmax, colmax, maxp)$data},error=function(e){return(NA)})
    if(!is.na(myknn.imp[1])){
      sim[loc,]=myknn.imp[1,]
    }
  }

  sim=impute::impute.knn(sim,k,rowmax,colmax,maxp)
  return(sim$data)
}

impute.SLR=function(sim,nbrmat,impseq,corrmat){
  for(loc in impseq){
    mis=which(is.na(sim[loc,]))
    nbr=which(nbrmat[loc,]>0)
    ypre=apply(sim[nbr,],1,function(s){
      z=s[mis]
      del=c(which(is.na(s)),mis)
      x=s[-del]
      y=sim[loc,-del]
      a=z*sum(x*y)/sum(x*x)
      return(a)
    })
    ypre=matrix(ypre,nrow=length(mis))
    sim[loc,mis]=apply(ypre,1,function(x){
      naloc=which(is.na(x))
      if(length(naloc)>0){
        sum(x[-naloc]*corrmat[loc,nbr][-naloc]/sum(corrmat[loc,nbr][-naloc]))
      }else{
        sum(x*corrmat[loc,nbr]/sum(corrmat[loc,nbr]))
      }
    })
  }
  return(sim)
}

impute.BPCA=function(sim,nbrmat,impseq,nPcs){
  for(loc in impseq){
    nbrs=which(nbrmat[loc,]>0)
    pc=tryCatch(pcaMethods::pca(t(sim[c(loc,nbrs),]), method="bpca", nPcs=nPcs,verbose=F),error=function(e){return(NA)})
    if(class(pc)=="pcaRes"){
      sim[loc,]=t(pcaMethods::completeObs(pc))[1,]
    }
  }
  return(sim)
}

impute.SVD=function(sim,nbrmat,impseq,SVDMaxiter=80){
for(loc in impseq){
    nbrs=which(nbrmat[loc,]>0)
    res=bcv::impute.svd(sim[c(loc,nbrs),],maxiter=SVDMaxiter)
    sim[loc,]=res$x[1,]
  }
  return(sim)
}

MINMA=function(dat.exp,
               dat.graph=NULL,
               dat.match=NULL,
               dat.ions=NULL,
               dat.adduct=NULL,
               logtransform=T,
               ionMode="+",
               ionNames=c("H","NH4","Na","K","2Na-H"),
               mz.eps=1e-5,
               t.eps=100,
               missRateUpperBound=0.2,
               nbrLevel=1,
               num.bcov=10,
               num.dcol=10,
               num.knn=10,
               NetSVR=T,
               SVR.k=10,
               SVR.rowmax=0.5,
               SVR.colmax=0.8,
               SVR.maxp=1000,
               NetKNN=F,
               KNN.k=10,
               KNN.rowmax=0.5,
               KNN.colmax=0.8,
               KNN.maxp=1500,
               NetSLR=F,
               NetBPCA=F,
               BPCA.nPCs=2,
               NetSVD=F,
               SVD.Maxiter=10,
               SVI=F,
               SVI.methods=c("HalfMin","Median","Mean"))
{
  if(is.null(dat.graph)){
    stop("Please provide Network information as an adjacency matrix.")
  }
  if(is.null(dat.ions)){
    stop("Please provide m/z & RT information in matrix form.")
  }
  if(is.null(dat.adduct)){
    stop("Please provide adduct ion dataframe.")
  }
  if(is.null(dat.match)){
    stop("Please provide the matching info.")
  }

  dat.exp[is.na(dat.exp)]=0
  if(logtransform){dat.exp=log(dat.exp+1)}
  dat.exp=expDatFilter(dat.exp,missUpperRate=missRateUpperBound)
  dat.exp[dat.exp==0]=NA

  print("Step1: data-preprocessing done!")


  # prepare network information
  nFeature=nrow(dat.exp)

  sim.FM=F_Map_M(dat.exp,dat.match,cgmat=dat.graph)
  refvec=ionMass(dat.adduct,ionNames)
  sim.mzMat=mzMat(edat=dat.exp,mzdat=dat.ions,refv=refvec,cutoff=mz.eps,tcutoff=t.eps)
  sim.FF=FFMat(edat=dat.exp,ch2cgmat=sim.FM,matchdat=dat.match,neilev=nbrLevel,cgmat=dat.graph)
  sim.FF=sim.FF$FFMat
  netinfor=networkInfor(dat.exp,sim.FF,sim.mzMat,bcov.num=num.bcov,dcol.num=num.dcol,knn.num=num.knn)

  print("Step2: predictor network is constructed!")

  impute.res=list()
  impute.res$filteredExpDat=dat.exp

  if(NetSVR){
    ptm=proc.time()
    res=impute.SVR(sim=dat.exp,nbrmat=netinfor$nbrMat,impseq=netinfor$impSeqRefVec,SVR.k,SVR.rowmax,SVR.colmax, SVR.maxp)
    tt=proc.time()-ptm
    impute.res$NetSVR=list(impMat=res,exeTime=tt[3])

    print("Imputation: NetSVR done!")
  }

  if(NetKNN){
    ptm=proc.time()
    res=impute.KNN(sim=dat.exp,nbrmat=netinfor$nbrMat,impseq=netinfor$impSeqRefVec,k=KNN.k,rowmax=KNN.rowmax,colmax=KNN.colmax, maxp=KNN.maxp)
    tt=proc.time()-ptm
    impute.res$NetKNN=list(impMat=res,exeTime=tt[3])

    print("Imputation: NetKNN done!")
  }

  if(NetSLR){
    ptm=proc.time()
    res=impute.SLR(sim=dat.exp,nbrmat=netinfor$nbrMat,impseq=netinfor$impSeqRefVec,corrmat=netinfor$weights)
    tt=proc.time()-ptm
    impute.res$NetSLR=list(impMat=res,exeTime=tt[3])

    print("Imputation: NetSLR done!")
  }

  if(NetBPCA){
    ptm=proc.time()
    res=impute.BPCA(sim=dat.exp,nbrmat=netinfor$nbrMat,impseq=netinfor$impSeqRefVec,nPcs=BPCA.nPCs)
    tt=proc.time()-ptm
    impute.res$NetBPCA=list(impMat=res,exeTime=tt[3])

    print("Imputation: NetBPCA done!")
  }

  if(NetSVD){
    ptm=proc.time()
    res=impute.SVD(sim=dat.exp,nbrmat=netinfor$nbrMat,impseq=netinfor$impSeqRefVec,SVDMaxiter=SVD.Maxiter)
    tt=proc.time()-ptm
    impute.res$NetSVD=list(impMat=res,exeTime=tt[3])

    print("Imputation: NetSVD done!")
  }

  if(SVI){
    if(is.element("Mean",SVI.methods)){
      ptm=proc.time()
      res=dat.exp
      res[is.na(res)]=mean(dat.exp,na.rm=T)
      tt=proc.time()-ptm
      impute.res$SVI.Mean=list(impMat=res,exeTime=tt[3])

      print("Imputation: replace by Mean done!")
    }

    if(is.element("HalfMin",SVI.methods)){
      ptm=proc.time()
      res=dat.exp
      res[is.na(res)]=min(dat.exp,na.rm=T)*0.5
      tt=proc.time()-ptm
      impute.res$SVI.HalfMin=list(impMat=res,exeTime=tt[3])

      print("Imputation: replace by Min/2 done!")
    }

    if(is.element("Median",SVI.methods)){
      ptm=proc.time()
      res=dat.exp
      res[is.na(res)]=stats::median(dat.exp,na.rm=T)
      tt=proc.time()-ptm
      impute.res$SVI.Median=list(impMat=res,exeTime=tt[3])

      print("Imputation: replace by Median done!")
    }
  }

  return(impute.res)
}




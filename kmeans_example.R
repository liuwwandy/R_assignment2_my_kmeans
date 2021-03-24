library("ggplot2") 
set.seed(100401544) 
a=c(rnorm(60,5,1), rnorm(50,15,1), rnorm(60,2,1), rnorm(60,3,1), rnorm(50,13,1)) 
b=c(rnorm(60,5,1), rnorm(50,26,1), rnorm(60,20,1), rnorm(60,30,1), rnorm(50,13,1)) 
x=data.frame(a,b)
plot(a,b)

kmeans_Jun=function(x,k=2,max_iter=100,distance="euclidean",n=5,visualize=TRUE){ 
  #Initializations 
  x1=x[,1] 
  y1=x[,2] 
  col=0 
  med_x=0 
  med_y=0 
  new_kx=c() 
  new_ky=c() 
  k_n=numeric(length(x1)) 
  clusters=numeric(length(x1))
  puntos=c(1:length(x1)) 
  datos=data.frame(puntos,x1,y1) 
  dis_tot=c() 
  datos_opt=data.frame() 
  centroids_opt=data.frame() 
  suma_total_new=0
  suma_total_old=100000000 
  #I set the limits for the random generated points between the minimum and maximum of x 
  lim_min_x1=min(x[,1]) 
  lim_max_x1=max(x[,1]) 
  lim_min_y1=min(x[,2]) 
  lim_max_y1=max(x[,2]) 
  #I avoid initialization 
  i=0 
  for(i in 1:n){ 
    new_kx=c() 
    new_ky=c() 
    #I generate the random points based on k
    ran_x=sample(lim_min_x1:lim_max_x1,k) 
    ran_y=sample(lim_min_y1:lim_max_y1,k) 
    old_kx=ran_x 
    old_ky=ran_y 
    it=1 
    #Different distance methods 
    while(it<=max_iter){ 
      if(distance=="euclidean"){ 
        #Euclidean Distance 
        euc_dist=numeric(length(x1)) 
        mat_dist=numeric() 
        i=0 
        j=0 
        for(i in 1:length(old_kx)){for(j in 1:length(x1)){ 
          euc_dist[j]=sqrt((x1[j]-old_kx[i])^2+(y1[j]-old_ky[i])^2) 
        }
          mat_dist=cbind(mat_dist,euc_dist) 
        } 
      } 
      if(distance=="squared"){ 
        #Squared Euclidean distance 
        squ_dist=numeric(length(x1)) 
        mat_dist=numeric() 
        for(i in 1:length(old_kx)){for(j in 1:length(x1)){ 
          squ_dist[j]=(x1[j]-old_kx[i])^2+(y1[j]-old_ky[i])^2 
        } 
          mat_dist=cbind(mat_dist,squ_dist) 
        } 
      }
      
      if(distance=="manhattan"){ 
        #Manhattan distance 
        manh_dist=numeric(length(x1)) 
        mat_dist=numeric() 
        for(i in 1:length(old_kx)){for(j in 1:length(x1)){ 
          manh_dist[j]=abs(x1[j]-old_kx[i])+abs(y1[j]-old_ky[i]) 
        } 
          mat_dist=cbind(mat_dist,manh_dist) 
        } 
      }
      if(distance=="maximum"){ 
        #Maximum distance 
        max_dist=numeric(length(x1)) 
        mat_dist=numeric() 
        for(i in 1:length(old_kx)){for(j in 1:length(x1)){ 
          dis1=abs(x1[j]-old_kx[i]) 
          dis2=abs(y1[j]-old_ky[i])
          max_dist[j]=max(dis1,dis2) 
        } 
          mat_dist=cbind(mat_dist,max_dist) 
        } 
      } 
      
      #Create clusters
      #Each column is a centroid and it has a 1 if the point is associated to that centroid 
      datos=data.frame(puntos,x1,y1) 
      g=0 
      for(g in 1:k){ 
        for (j in 1:length(mat_dist[,1])){ 
          if(which.min(mat_dist[j,])==g){ 
            k_n[j]=1 
          }else{ 
            k_n[j]=0 
          } 
        } 
        #I add all the columns in a data frame 
        datos=data.frame(datos,k_n) 
      } 
      #I compute the mean of the points associated to each centroid 
      j=0 
      new_kx=c() 
      new_ky=c() 
      col=0 
      unx=c() 
      for(j in 1:k){ 
        col=j+3
        unx=(datos[,col]>0) 
        unnx=datos[,col][unx] 
        if(length(unnx)==0){ 
          new_kx[j]=old_kx[j] 
        }else{ 
          med_x=sum(datos[,2]*datos[,col])/length(unnx) 
          new_kx=c(new_kx,med_x) 
        } 
        uny=(datos[,col]>0) 
        unny=datos[,col][uny] 
        if(length(unny)==0){ 
          new_ky[j]=old_ky[j]
        }else{ 
          med_y=sum(datos[,3]*datos[,col])/length(unny) 
          new_ky=c(new_ky,med_y) 
        } 
      } 
      #New centroid and control if centroids don't move 
      control=0 
      control=sum(new_kx-old_kx)+sum(new_ky-old_ky) 
      if(control==0){ 
        cat("Centroids didn't move","\n") 
        break 
      }else{ 
        i=0 
        col=0
        clusters=numeric(length(x1)) 
        k_color=numeric(length(x1)) 
        for(i in 1:k){ 
          col=3+i 
          k_color=datos[,col]*i 
          clusters=clusters+k_color 
        } 
        #Actualize data frames to plot 
        datos=data.frame(datos,clusters) 
        centroids=data.frame(new_kx,new_ky) 
        #Plot
        p1=ggplot(datos)+aes(datos[,2],datos[,3],color=factor(clusters))+
          geom_point(data=datos,aes(datos[,2],datos[,3]))+
          geom_point(data=centroids,aes(new_kx,new_ky),color='black')+
          labs(x="X",y="Y",title="Clusters & Centroids (Kmeans)")
        
        if(visualize==TRUE){ 
          Sys.sleep(0.5) 
          print(p1) 
        } 
        cat("Iteration ", it, "\n") 
        old_kx=new_kx 
        old_ky=new_ky 
        #cat("Coordinate x of centroids", new_kx, "\n") 
        #cat("Coordinate y of centroids", new_ky, "\n") 
        it=it+1 
      } 
    }
    g=0 
    tot=c()
    for (j in 1:length(mat_dist[,1])){ 
      tot=c(tot,min(mat_dist[j,])) 
    } 
    #Take the best configuration 
    suma_total_new=sum(tot) 
    if(suma_total_new<suma_total_old){ 
      datos_opt=data.frame(datos[,2],datos[,3]) 
      centroids_opt=data.frame(centroids[,1],centroids[,2]) 
      p_opt1=ggplot(datos)+aes(datos[,2],datos[,3],color=factor(clusters))+
        geom_point(data=datos,aes(datos[,2],datos[,3]))+
        geom_point(data=centroids,aes(new_kx,new_ky),color='black')+
        labs(x="X",y="Y",title="Clusters & Centroids (Optimum Kmeans)") 
      suma_total_old=suma_total_new 
    } 
    dis_tot=c(dis_tot,sum(tot))
  } 
  p_opt=ggplot(datos_opt)+aes(datos_opt[,1],datos_opt[,2],color=factor(clusters))+
    geom_point(data=datos_opt,aes(datos_opt[,1],datos_opt[,2]) )+
    geom_point(data=centroids_opt,aes(centroids_opt[,1],centroids_opt[,2]),color='black')+
    labs(x="X",y="Y",title="Clusters & Centroids (Optimum Kmeans)") 
  cat("The minimum distance for", n,"configurations is",min(dis_tot), "\n") 
  print(p_opt1) 
} 
kmeans_Jun(x,k=5)


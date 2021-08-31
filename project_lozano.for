c**********************************************************************
c   Stellar plotting
c   FFT frac=28.0
c**********************************************************************

	program project
	real wave,inten,yfr,ymin,ymax
	dimension wave(65536),inten(65536),yfr(65536)
	real responsefft(65536),trucW(65536),trucInt(65536)
	real X1current(100),X2current(100),Y1current(100),Y2current(100)
	real drawRX(2),drawRY(2)
	character*80 head0,head1,blank,device_type*40,filename*40
	character*1 inpUserC
	real intervalX,checkX,checkYPo,XVC1,XVC2,YVC1,YVC2,fftXM
	real valFFTfr,topValueY,bottomValueY,WLL,WLH,intervalY
	integer pgcurs,pgband, NPT,index1,index2,newTp,color,Tcolo,gridVal
	integer sizeBuff,sizeDrawR
	character*1 ika, inputUser
	logical*1 hardcopy,hardcopy_done,plotminmax,fastOn,actiGrid
	logical*1 initialSet
	common/copy/hardcopy,hardcopy_done
	common/pdevice/device_type,length_device
	do i=1,80
	   blank(i:i)=' '
	end do
	head0=blank
	head1=blank
	hardcopy=.false.
	hardcopy_done=.false.
	plotminmax=.true.
	fastOn=.false.
	actiGrid=.false.
	initialSet=.true.
	color=1
	gridVal=1
	sizeBuff=1
	sizeDrawR=2
	valFFTfr=28.0
	

c**********************************************************************

	write(6,*)'Enter File Name'
	read(5,*)filename
	open(unit=15,file=filename)

c	Read first two title lines in file
	read(15,*) head0
	read(15,*) head1
	print *, 'Test try to see what ', head1
	
c	Find number of points in file
	n=0
   10	n=n+1
	read(15,*,end=100)
	go to 10
  100 	npoints  = n-1
	write(6,*) 'Number of points in file is ',npoints
	close(15)
	
c		Read data file
	open(unit=15,file=filename)
	read(15,*) head0
	read(15,*) head1
	do i=1,npoints
	   read(15,*) wave(i),inten(i)
	end do	

	YMAX=-1000000.
	YMIN= 1000000.
	do i=1,npoints
		if(inten(i).gt.YMAX) YMAX=inten(I)
		if(inten(i).lt.YMIN) YMIN=inten(i)
	end do
	write(6,*) wave(1),wave(npoints)
	write(6,*)YMAX,YMIN
	close(15)
	
	intervalX=5.0
	intervalY=0.01
	gridVal=1
	valFFTfr=28.0

c**********************************************************************
	
	call pgbeg(0,'?',1,1)
	call pgask(.false.)  
	call pgqinf('DEV/TYPE',device_type,length_device)
	write(6,*)'Original device type is ',device_type
	write(6,*)' with length ',length_device


c	Set up window	
   99	WLL=wave(1)
	WLH=wave(npoints)
	bottomValueY=YMIN-0.001
	topValueY=YMAX+0.01
c	Plot from zero to ymax
   16	if(.not.plotminmax)then
	    	yl=0.
		else
	    	yl=ymin
        end if
	call pgenv(WLL,WLH,bottomValueY,topValueY,0,gridVal)
c	call pgvsiz(0.0,18.0,0.0,10.0)
	call pglab('Wavelength [\A]','Intensity','Spectrum')
	call pgsch(0.8)
c	skip labelling of commnads for hardcopy
	if(hardcopy)go to 778
	call pgmtxt('T',1.0,0.0,0.0,'E=Exit H=Hardcopy I=Reset F= FFT')
	call pgmtxt('T',2.0,0.0,0.0,'M=Max to Min Y=Shift(Downward)')
	call pgmtxt('T',3.0,0.0,0.0,'Z=Zoom in C=Color T=Shift(Upward)')
	call pgmtxt('T',4.0,0.0,0.0,'X=Zoom out Q=Section FFT C=Color')
	call pgmtxt('T',1.0,0.7,0.0,'O=Change frac R=Shift(right)')
	call pgmtxt('T',2.0,0.7,0.0,'P=Find Coordinate L=Shift(left)')
	call pgmtxt('T',3.0,0.7,0.0,'D=Draw Polyline N=Help G=grid')
	call pgmtxt('T',4.0,0.7,0.0,'B=Signal/Noise S=Sign/Noise Section')
	call pgsch(1.0)	
  778	call pgiden

c	Plot data in graph	
  801	call plotGraph(wave,inten,npoints,color)
	call pgsah(2,20.0,0.5)
	
	if(initialSet)then
		X1current(sizeBuff)=WLL
		X2current(sizeBuff)=WLH
		Y1current(sizeBuff)=bottomValueY
		Y2current(sizeBuff)=topValueY
		initialSet=.false.
	end if
	
	if(fastOn)then
		call plotGraph(wave,yfr,npoints,2)
		call pgsci(1)
	end if

c	Skip cursor input during hardcopy
	if(hardcopy)go to 779
c	Cursor commands
  13	ireq=pgband(7,1,0,0,x1,y1,ika)
	write(6,*) 'Command clicked ', ika
	if(ika.eq.'e'.or.ika.eq.'E') goto 999
	if(ika.eq.'h'.or.ika.eq.'H') goto 900
	if(ika.eq.'m'.or.ika.eq.'M') goto 800
	if(ika.eq.'z'.or.ika.eq.'Z') then
		temp=pgband(2,1,x1,y1,CUX,CUY,inputUser)
		if(x1.gt.CUX)then
			WLL=CUX
			WLH=x1
		else
			WLL=x1
			WLH=CUX
		end if
	
		if(y1.gt.CUY)then
			bottomValueY=CUY
			topValueY=y1
		else
			bottomValueY=y1
			topValueY=CUY
		end if
		call pgsci(color)
		sizeBuff=sizeBuff+1
		Y1current(sizeBuff)=bottomValueY
		Y2current(sizeBuff)=topValueY
		X1current(sizeBuff)=WLL
		X2current(sizeBuff)=WLH
		go to 16
	end if
	if(ika.eq.'x'.or.ika.eq.'X') then
		if(sizeBuff.eq.1)then
			WLL=wave(1)
			WLH=wave(npoints)
			bottomValueY=YMIN-0.001
			topValueY=YMAX+0.01
			write(6,*) 'Zoom in first'
		else
			sizeBuff=sizeBuff-1
			bottomValueY=Y1current(sizeBuff)
			topValueY=Y2current(sizeBuff)
			WLL=X1current(sizeBuff)
			WLH=X2current(sizeBuff)
		end if 
		go to 16
	end if
	if(ika.eq.'i'.or.ika.eq.'I') then
		fastOn=.false.
		sizeBuff=1
		go to 99
	end if
	if(ika.eq.'f'.or.ika.eq.'F') then
		call pgmtxt('B',-2.0,0.0,0.0,'Click F->erase the FFT')
		write(6,*)'The current value of frac is: ',valFFTfr
        if(fastOn)then
			fastOn=.false.
			go to 16
		else
			call smfft(wave,inten,yfr,npoints,valFFTfr,sigma)
			call plotGraph(wave,yfr,npoints,color+1)
			call pgsci(color)
			fastOn=.true.
		end if
	end if
	if(ika.eq.'l'.or.ika.eq.'L') then
		checkX=WLL-intervalX
		if(checkX.lt.wave(1))then
			write(6,*)'End of graph'
		else
			WLL=checkX
			WLH=WLH-intervalX
			go to 16
		end if 
	end if
	if(ika.eq.'r'.or.ika.eq.'R') then
		checkX=WLH+intervalX
		if(checkX.gt.wave(npoints))then
			write(6,*)'End of graph'
		else
			WLL=WLL+intervalX
			WLH=checkX
			go to 16
		end if 
	end if
	if(ika.eq.'t'.or.ika.eq.'T') then
		checkYPo=topValueY+intervalY
		if(checkYPo.gt.YMAX)then
			write(6,*)'End of graph'
		else
			bottomValueY=bottomValueY+intervalY
			topValueY=checkYPo
			go to 16
		end if 
	end if
	if(ika.eq.'y'.or.ika.eq.'Y') then
		checkYPo=bottomValueY-intervalY
		if(checkYPo.lt.YMIN)then
			write(6,*)'End of graph'
		else
			bottomValueY=checkYPo
			topValueY=topValueY-intervalY
			go to 16
		end if 
	end if
	if(ika.eq.'q'.or.ika.eq.'Q') then
		call pgsci(color+4)
		write(6,*)'The current value of frac is: ',valFFTfr
		temp=pgband(1,1,x1,y1,CUX,CUY,inputUser)
		if(x1.gt.CUX)then
			fftXM=x1
			x1=CUX
			CUX=fftXM
		end if
		call valCoor(wave,inten,x1,CUX,index1,index2)
		XVC1=wave(index1)
		XVC2=wave(index2)
		YVC1=inten(index1)
		YVC2=inten(index2)
		call pgarro(XVC1,YVC1,XVC2,YVC2)
		call calcInter(wave,inten,index1,index2,trucW,trucInt)
		newTp =index2-index1+1
		call smfft(trucW,trucInt,responsefft,newTp,valFFTfr,sigma)
		call plotGraph(trucW,responsefft,newTp,color+2)
		call pgsci(color)
	end if
	if(ika.eq.'s'.or.ika.eq.'S') then
		call pgsci(color+5)
		temp=pgband(1,1,x1,y1,CUX,CUY,inputUser)
		if(x1.gt.CUX)then
			fftXM=x1
			x1=CUX
			CUX=fftXM
		end if
		call valCoor(wave,inten,x1,CUX,index1,index2)
		XVC1=wave(index1)
		XVC2=wave(index2)
		YVC1=inten(index1)
		YVC2=inten(index2)
		call pgarro(XVC1,YVC1,XVC2,YVC2)
		call pgsci(color)
		call calcInter(wave,inten,index1,index2,trucW,trucInt)
		newTp =index2-index1+1
		call calcSigToNoi(newTp,trucW,trucInt)
		bottomValueY=Y1current(sizeBuff)
		topValueY=Y2current(sizeBuff)
		WLL=X1current(sizeBuff)
		WLH=X2current(sizeBuff)
	end if
	if(ika.eq.'b'.or.ika.eq.'B') then
		call calcSigToNoi(npoints,wave,inten)
	end if
	if(ika.eq.'c'.or.ika.eq.'C') then
		call pgmtxt('B',-2.0,0.1,0.0,'Refer to console')
		write(6,*)'Type a number from 1 to 10 (Needs to be an integer)'
		read(5,*)Tcolo		
		color=Tcolo
		call pgsci(color)
		go to 16
	end if
	if(ika.eq.'g'.or.ika.eq.'G') then
        if(actiGrid)then
			actiGrid=.false.
			gridVal=1
		else
			actiGrid=.true.
			gridVal=2
		end if
		go to 16		
	end if
	if(ika.eq.'d'.or.ika.eq.'D') then
		call pgsci(color+5)
		temp=pgband(1,1,x1,y1,CUX,CUY,inputUser)
		drawRX(1)=x1
		drawRX(2)=CUX
		drawRY(1)=y1
		drawRY(2)=CUY
		call pgsfs(3)
		call pgline(sizeDrawR,drawRX,drawRY)
		call pgsci(color)  	
	end if
	if(ika.eq.'p'.or.ika.eq.'P') then
		call pgmtxt('B',-3.0,0.1,0.0,'Please select the point')
		call pgmtxt('B',-2.0,0.1,0.0,'and check console for output.')
		temp=pgband(7,1,x1,y1,CUX,CUY,inputUser)
		write(6,*) 'Current position in x: ',CUX
		write(6,*) 'Current position in y:',CUY
		go to 16
	end if
	if(ika.eq.'N'.or.ika.eq.'n') then
		call pgmtxt('B',-2.0,0.0,0.0,'Please check console for output.')
		write(6,*) 'Available commands:'
		write(6,*) 'E->  Close the program'
		write(6,*) 'H->  Produce a hardcopy'
		write(6,*) 'I->  Return to the initial graph.'
		write(6,*) 'M->  Replot from max to min'
		write(6,*) 'R->  Shift the graph to the right'
		write(6,*) 'L->  Shift the graph to the left'
		write(6,*) 'T->  Shift the graph upwards'
		write(6,*) 'Y->  Shift the graph downwards'
		write(6,*) 'Z->  Zoom in a section of the graph'
		write(6,*) 'G->  Produce a grid layout'
		write(6,*) 'X->  Zoom out'
		write(6,*) 'F->  Trace a FFT with all data points'
		write(6,*) 'Q->  Produce a FFT in a section of the graph.'
		write(6,*) 'C->  Change the color.'
		write(6,*) 'D-> Draw a Polyline(Lines with the same origin.)'
		write(6,*) 'O-> Change the value for frac.'
		write(6,*) 'B-> Calcualte the signal to noise ratio.'
		write(6,*) 'S-> Calcualte sig/noise in section of the graph.'
		write(6,*) 'H-> Request information about the commands.'
		write(6,*) '---------------------------------------------'
		write(6,*) 'Additional information:'
		write(6,*) '*Commands S and Q are the same as B and F'
		write(6,*) ' with the difference that you can select a specific'
		write(6,*) ' section in the graph'
		write(6,*) '*To make the graph occupy your whole screen.Increase'
		write(6,*) ' the size of your screen and call the command I'
		write(6,*)'Do you want to close Help:(Y for Yes)'
		read(5,*)inpUserC
		go to 16
	end if
	if(ika.eq.'o'.or.ika.eq.'O') then
		write(6,*)'The old value of frac is: ',valFFTfr
		write(6,*)'Please insert the new value for frac: '
		write(6,*)'Note: Make sure that the input inserted is '
		write(6,*)'	a number otherwise an error will occur. '
		read(5,*)valFFTfr
		write(6,*)'Frac now is: ',valFFTfr
	end if
c	*****************************************************	
	write(6,*) 'Not an available command, enter again'
	goto 13

c	Hardcopy setup
  900	call makehard('/CPS')
        write(6,*)'Calling makehard'
        write(6,*)'Retaining original device type ',device_type
        write(6,*)' with length ',length_device
	go to 16
c	*****************************************************
  779	hardcopy_done=.true.
	hardcopy=.false.

c	Reset device type back to original setting
        if(hardcopy_done)then
           write(6,*)'Hard copy has been done'
           call pgbeg(0,device_type(1:length_device),1,1)
           write(6,*)'Re-establishing original device type ',device_type
           write(6,*)' with length ',length_device
           hardcopy=.false.
           hardcopy_done=.false.
           call pgask(.false.)
	end if 
	go to 16

c 	Toggle plot between 0 to max and min to max
  800	if(plotminmax)then
	   plotminmax=.false.
	else
	   plotminmax=.true.
	end if
	go to 16
  999	call pgend
	stop
	end

c**********************************************************************
	   subroutine makehard(string)
c**********************************************************************
      character*(*) string,device_type*40
      logical*1 hardcopy,spectrum_plotted,fastOn
      common/copy/hardcopy,spectrum_plotted
      common/pdevice/device_type,length_device
      l=len(string)
      call pgbegin(0,string(1:l),1,1)
      call pgask(.false.)
      hardcopy=.true.
      write(6,*)'Beginning hardcopy process'
      return
      end

c**********************************************************************
	   subroutine valCoor(wave,inten,x1,CUX,index1,index2)
c**********************************************************************
      real wave,inten
	  dimension wave(65536),inten(65536)
	  real x1,CUX,tolerance
	  character*1 ika, inputUser
	  integer index1,index2
	  logical*1 searchingF, searchingS
	  
	  searchingF=.true.
	  searchingS=.true.
		
	  i=1
	  do while(searchingS)
	  	if(wave(i).gt.x1.and.searchingF)then
			index1 =i
			searchingF=.false.
		end if
	  	if(wave(i).gt.CUX.and.searchingS)then
			index2 =i
			searchingS=.false.
		end if
		i=i+1
	  end do
	  
	  return
      end

c**********************************************************************
	   subroutine plotGraph(coordinatesX,coordinatesY,ninput,color)
c**********************************************************************
      real coordinatesX,coordinatesY
	  dimension coordinatesX(5000),coordinatesY(5000)
	  integer ninput, color,lasColor

	  do i=1,ninput-1
		   call pgmove(coordinatesX(i),coordinatesY(i))
		   call pgsci(color)	
		   call pgdraw(coordinatesX(i+1),coordinatesY(i+1))
	  end do
	  call pgupdt
	  lasColor=color
	  call pgsci(color)
	  return
      end

**********************************************************************
	  subroutine calcInter(wave,inten,index1,index2,trucW,trucInt)
c**********************************************************************
	  real inten,wave,x1,y1
	  dimension inten(65536),wave(65536)
	  real trucW(65536),trucInt(65536)
	  integer index1,index2,newTp

	 	newTp=1

	 	do i=index1,index2
			trucW(newTp)=wave(i)
			trucInt(newTp)=inten(i)
			newTp=newTp+1
	 	end do 
	 	newTp=newTp-1
	  return
      end
c**********************************************************************
	  subroutine calcSigToNoi(newIpoi,trunWave,truncInte)
c**********************************************************************
	  real truncInte,trunWave,truRet
	  dimension truncInte(65536),trunWave(65536),truRet(65536)
	  real truncInteInt(65536)
	  real signal,noise,lowestI,higestI,siToNo,ave,percA,avSig
	  real newInten(65536)
	  integer newIpoi
	  
	  higestI=-1000000.
	 
	  lowestI= 1000000.
	  ave=0.0

c----------------Signal------------------------------------
      do i=1,newIpoi
		if(truncInte(i).gt.higestI) higestI=truncInte(i)
		if(truncInte(i).lt.lowestI) lowestI=truncInte(i)
	  end do
	  percA=(higestI-lowestI)*0.1
	  j=1
	  do i=1,newIpoi
			if(truncInte(i).gt.percA)then	
				truncInteInt(j)=truncInte(i)
				j=j+1
			end if
	   end do
	   avSig=sum(truncInteInt)/j
	  signal = avSig
c----------------Noise-----------------------------------
	
	  call smfft(trunWave,truncInte,truRet,newIpoi,28.0,sigma)
	  do k=1,newIpoi
		newInten(k)= abs(truRet(k)-truncInte(k))
	  end do
	  ave=sum(newInten) 
	  noise=ave/newIpoi
	  siToNo=signal/noise

	  write(6,*)'Number of points ',newIpoi
	  write(6,*)'Signal ',signal
	  write(6,*)'Noise ',noise
	  write(6,*)'Signal to Noise ',siToNo
	  return
      end

c**********************************************************************
      subroutine smfft(x,y,yfr,nobs,frac,sigma)
c**********************************************************************
      parameter(ix_dim=65536,iy_dim=65536)
c     implicit real (a-h,o-z)
      real*8 davg,dfr(ix_dim),dfi(ix_dim),dw(ix_dim)
      real*8 dw1(ix_dim),dy_temp(ix_dim),dpi,dtwopi
      real x,y,yfr,frac,sigma,res(ix_dim)
      dimension x(ix_dim),y(ix_dim),yfr(ix_dim)
      dpi = 3.1415926535897932d0
      dtwopi = 2.d0*dpi
c     nfft = 4096
      nfft = 8192
c
c        Modification to handle columns without having to produce a specialized subroutine
c
      if(nobs.lt.1000)then
          nfft=1024
      else if(nobs.gt.100000)then
          nfft=262144
      end if
c     write(6,*)'Value of frac for window function = ',frac
c Window size in terms of FFT size...
      nf = (frac/100.)*nfft
c Generate Hanning window function...
      do k=1,nf*2
          dw(k) = 0.5d0 - 0.5d0*dcos(dtwopi*k/(2*nf))
      end do
c Zero some arrays...
      do k=1,nfft
          dfr(k) = 0.d0
          dfi(k) = 0.d0
          dw1(k) = 0.d0
      end do
c Rearrange window into larger array, and in wrap-around order for use with FFT later...
      do i=1,nf
          dw1(i) = dw(nf+i)
      end do
      do i=1,nf
          dw1(i+nfft-nf) = dw(i)
      end do
      ntaper=nobs/12
      if(ntaper.lt.32)ntaper=32
      npnt=nobs+2*ntaper
c
c Pad data with ntaper values at start and end; then remove average from data...
c
      davg = 0.d0
	  iq1=nobs-4
	  iq2=nobs-3
	  iq3=nobs-2
	  iq4=nobs-1	
      do j=1,npnt
          if(j.le.ntaper)dy_temp(j)=0.2*(y(6)+y(7)+y(3)+y(4)+y(5))
          if(j.ge.npnt-ntaper) THEN
		      dy_temp(j)=0.2*(y(iq1)+y(iq2)+y(iq3)+y(iq4)+y(nobs))
          end if
          if(j.gt.ntaper.and.j.lt.npnt-ntaper)dy_temp(j)=y(j-ntaper)
          davg = davg + dy_temp(j)
      end do
c     write(6,*)'After padding, dy_temp(1600)=',dy_temp(1600)
      davg = davg/npnt
c     write(6,*)'Average value = ',davg
      do j=1,npnt
          dy_temp(j) = dy_temp(j) - davg
      end do
c     write(6,*)'After average subtraction, dy_temp(1600)=',dy_temp(1600)
c
c Taper the data.  Default is 1/12 at each end.  Dummy data are added and tapered so that no spectrum is lost (spectrum is set
c to the values at the extremes of the spectrum sydumelected in the input file
c
      call taper(dy_temp,npnt,ntaper)
c also write data into larger array for FFT use...
c     write(6,*)'After taper, dy_temp(1600)=',dy_temp(1600)
      do j=1,npnt
          dfr(j) = dy_temp(j)
      end do
c     write(6,*)'After filling larger array, dfr(1600)=',dfr(1600)
c Compute forward FFT...
c     print *,' Computing FFT...'
      call fastf(dfr,dfi,nfft)
c     write(6,*)'After forward fft, dfr(1600), dfi(1600) =',dfr(1600),dfi(1600)
c Now window transform...
      do k=1,nfft
          dfr(k) = dfr(k)*dw1(k)/nfft
          dfi(k) = dfi(k)*dw1(k)/nfft
      end do
c     write(6,*)'After windowing fft, dfr(1600), dfi(1600) =',dfr(1600),dfi(1600)
c Do inversion via alternate inversion theorem...
c     print *,' Computing inverse FFT...'
      do k=1,nfft
          dfi(k) = -dfi(k)
      end do
      call fastf(dfr,dfi,nfft)
      do k=1,nfft
          dfi(k) = -dfi(k)
      end do
c     write(6,*)'After inverse fft, dfr(1600), dfi(1600) =',dfr(1600),dfi(1600)
c Compute rms of "fit", where fr = smoothed data...
      sigma = 0.
      do j=1,nobs
          yfr(j)=dfr(j+ntaper)+davg
      end do
c     write(6,*)'After adding avg, yfr(1600-ntaper) =',yfr(1600-ntaper)
      do j=1,nobs
          res(j) = y(j) - yfr(j)
          sigma = sigma + res(j)*res(j)
      end do
      sigma = sqrt(sigma/(nobs-1))
c     print *,' sigma = +/- ',sigma
      return
      end

C THE FOLLOWING SIGN CONVENTION FOR THE FFT IS USED:
C               N-1
C   F(K) = (1/N)SUM [f(T) exp(-2*PI*K*T/N)]
C               T=0
C
      SUBROUTINE FASTF(FR,FI,N)
c      IMPLICIT REAL*8 (A-H,O-Z)
C
C N = NUMBER OF DATA POINTS = 2**N
C FR = REAL DATA SET
C FI = IMAGINARY PART OF DATA SET (=0.0 IF ONLY REAL)
C FIRST COMPUTE M...
C
c     REAL FR(N),FI(N)
      dimension fr(n),fi(n)
      real*8 pi,kd,fr,fi,gr,gi,eu,ez,er,ei,seu
      pi = 3.1415926535897932d0
c     PI=3.14159265
      M=0
      KD=N
1     KD=KD/2
      M=M+1
      IF(KD.GE.2)GO TO 1
      ND2=N/2
      NM1=N-1
      L=1
C
C SHUFFLE INPUT DATA IN BINARY DIGIT REVERSE ORDER...
C
      DO 4 K=1,NM1
      IF(K.GE.L)GO TO 2
      GR=FR(L)
      GI=FI(L)
      FR(L)=FR(K)
      FI(L)=FI(K)
      FR(K)=GR
      FI(K)=GI
2     NND2=ND2
3     IF(NND2.GE.L)GO TO 4
      L=L-NND2
      NND2=NND2/2
      GO TO 3
4     L=L+NND2
C
C FIRST ARRANGE COUNTING OF M STAGE...
C
      DO 6 J=1,M
      NJ=2**J
      NJD2=NJ/2
      EU=1.0d0
      EZ=0.0d0
      ER=dCOS(-PI/NJD2)
      EI=dSIN(-PI/NJD2)
C
C COMPUTE FOURIER TRANSFORM IN EACH M STAGE...
C
      DO 6 IT=1,NJD2
      DO 5 IW=IT,N,NJ
      IWJ=IW+NJD2
      GR=FR(IWJ)*EU-FI(IWJ)*EZ
      GI=FI(IWJ)*EU+FR(IWJ)*EZ
      FR(IWJ)=FR(IW)-GR
      FI(IWJ)=FI(IW)-GI
      FR(IW)=FR(IW)+GR
5     FI(IW)=FI(IW)+GI
      SEU=EU
      EU=SEU*ER-EZ*EI
6     EZ=EZ*ER+SEU*EI
      RETURN
      END
      SUBROUTINE TAPER(X,N,NTAPER)
c      IMPLICIT REAL*8 (A-H,O-Z)
      real*8 pi,pihalf,x
      DIMENSION X(1)
      pi = 3.1415926535897932d0
      pihalf = 0.5d0*3.1415926535897932d0
c     PIHALF=0.5*3.141593
      DO I=1,NTAPER
          X(I)=X(I)*dCOS(PIHALF*dFLOAT(NTAPER-I+1)/dFLOAT(NTAPER))
      end do
      NSTART = N-NTAPER+1
      DO I=NSTART,N
          X(I)=X(I)*dCOS(0.5d0*pi*dFLOAT(I-NSTART+1)/dFLOAT(NTAPER))
      end do
      RETURN
      END

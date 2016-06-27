      subroutine READ_CAMx_PT(ifile,NBIG1,NBIG2,NBIG3,
     &  XSTK,YSTK,HSTK,DSTK,TSTK,
     &  VSTK,KCELL,FLOW,PLMHT,HRS2,STK2,
     &  CONC,STK1,HRS1,SPECIES1)

      implicit none
      integer NBIG1,NBIG2,NBIG3
      character*200 ifile
      CHARACTER*4 FILNAM(10),FILEID(60)
      CHARACTER*4 SPECIEN(10)
      character, ALLOCATABLE :: MSPEC(:,:)
      REAL XSTK(NBIG3),YSTK(NBIG3),HSTK(NBIG3)
      REAL DSTK(NBIG3),TSTK(NBIG3),VSTK(NBIG3)
      REAL FLOW(NBIG2),PLMHT(NBIG2)
      INTEGER KCELL(NBIG2),HRS2(NBIG2),STK2(NBIG2)
      REAL CONC(NBIG1)
      INTEGER HRS1(NBIG1),SPECIES1(NBIG1),STK1(NBIG1)
      REAL BEGTIME, ENDTIME,plon,plat
      REAL XORG,YORG,DX,DY,tlat1,tlat2,rdum
      integer NX, NY, NZ, IDATA, IDATB, idum
      integer MAXS,istag,I,J,HR,L,NI,NSTK,IX



      open(9,file=ifile,status='old',form='unformatted',convert='big_endian')
c      open(9,file=ifile,status='old',form='unformatted')

      READ (9)FILNAM,FILEID,idum,MAXS,IDATA,BEGTIME,IDATB,ENDTIME
      READ (9) plon,plat,idum,XORG,YORG,DX,DY,NX,
     $ NY,NZ,idum,istag,tlat1,tlat2,rdum
      READ  (9) idum,idum, NX,NY

      ALLOCATE(MSPEC(10,MAXS))

      NI=0

      READ  (9) ((MSPEC(I,J),I=1,10),J=1,MAXS)

      READ  (9) idum, NSTK


      READ  (9) (XSTK(I),YSTK(I),HSTK(I),DSTK(I),TSTK(I),VSTK(I),
     &   I=1,NSTK) 

      DO HR=1,24

       IX = (HR-1)*NSTK
       READ(9) idum, rdum, idum, rdum
       READ(9) idum, NSTK
       READ(9) (idum,idum,KCELL(IX+I),FLOW(IX+I),PLMHT(IX+I),I=1,NSTK)
       DO I=1,NSTK
          HRS2(IX+I)=HR
          STK2(IX+I)=I
       END DO



       DO L=1,MAXS

c        READ(9) idum, SPECIEN, ((CONC(NI+I+(J-1)*NX),I=1,NX),J=1,NY)
        READ(9) idum, SPECIEN, (CONC(NI+I),I=1,NSTK)

         DO I=1,NSTK
         NI=NI+1
         STK1(NI)=I
         HRS1(NI)=HR
         SPECIES1(NI)=L
         ENDDO

       ENDDO

      ENDDO

      close(9)

      end


      subroutine READ_CAMx_IJ_STATIC(ifile,NBIG,IS,JS,SPECIES,
     &  CONC )

      implicit none
      integer NBIG
      character*200 ifile
      CHARACTER*4 FILNAM(10),FILEID(60)
      CHARACTER*4 SPECIEN(10)
      character, ALLOCATABLE :: MSPEC(:,:)
      REAL CONC(NBIG)
      INTEGER IS(NBIG),JS(NBIG),SPECIES(NBIG)
      REAL BEGTIME, ENDTIME,plon,plat
      REAL XORG,YORG,DX,DY,tlat1,tlat2,rdum
      integer NX, NY, NZ, IDATA, IDATB, idum
      integer MAXS,istag,I,J,L,NI


c      open(9,file=ifile,status='old',form='unformatted')
      open(9,file=ifile,status='old',form='unformatted',convert='big_endian')

      READ (9)FILNAM,FILEID,idum,MAXS,IDATA,BEGTIME,IDATB,ENDTIME
      READ (9) plon,plat,idum,XORG,YORG,DX,DY,NX,
     $ NY,NZ,idum,istag,tlat1,tlat2,rdum
      READ  (9) idum,idum, NX,NY

      ALLOCATE(MSPEC(10,MAXS))

      NI=0

      READ  (9) ((MSPEC(I,J),I=1,10),J=1,MAXS)


       READ(9) idum, rdum, idum, rdum

       DO L=1,MAXS

        READ(9) idum, SPECIEN, ((CONC(NI+I+(J-1)*NX),I=1,NX),J=1,NY)

        DO J=1,NY
         DO I=1,NX
         NI=NI+1
         IS(NI)=I
         JS(NI)=J
         SPECIES(NI)=L
         ENDDO
        ENDDO

       ENDDO


      close(9)

      end

      subroutine READ_CAMx_IJ(ifile,NBIG,IS,JS,HRS,SPECIES,
     &  CONC )

      implicit none
      integer NBIG
      character*200 ifile
      CHARACTER*4 FILNAM(10),FILEID(60)
      CHARACTER*4 SPECIEN(10)
      character, ALLOCATABLE :: MSPEC(:,:)
      REAL CONC(NBIG)
      INTEGER IS(NBIG),JS(NBIG),HRS(NBIG),SPECIES(NBIG)
      REAL BEGTIME, ENDTIME,plon,plat
      REAL XORG,YORG,DX,DY,tlat1,tlat2,rdum
      integer NX, NY, NZ, IDATA, IDATB, idum
      integer MAXS,istag,I,J,HR,L,NI


c      open(9,file=ifile,status='old',form='unformatted')
      open(9,file=ifile,status='old',form='unformatted',convert='big_endian')

      READ (9)FILNAM,FILEID,idum,MAXS,IDATA,BEGTIME,IDATB,ENDTIME
      READ (9) plon,plat,idum,XORG,YORG,DX,DY,NX,
     $ NY,NZ,idum,istag,tlat1,tlat2,rdum
      READ  (9) idum,idum, NX,NY

      ALLOCATE(MSPEC(10,MAXS))

      NI=0

      READ  (9) ((MSPEC(I,J),I=1,10),J=1,MAXS)

      DO HR=1,24

       READ(9) idum, rdum, idum, rdum

       DO L=1,MAXS

        READ(9) idum, SPECIEN, ((CONC(NI+I+(J-1)*NX),I=1,NX),J=1,NY)

        DO J=1,NY
         DO I=1,NX
         NI=NI+1
         IS(NI)=I
         JS(NI)=J
         HRS(NI)=HR
         SPECIES(NI)=L
         ENDDO
        ENDDO

       ENDDO

      ENDDO

      close(9)

      end

      subroutine READ_CAMx_IJK(ifile,NBIG,IS,JS,KS,HRS,SPECIES,
     &  CONC )

      implicit none
      integer NBIG
      character*200 ifile
      CHARACTER*4 FILNAM(10),FILEID(60)
      CHARACTER*4 SPECIEN(10)
      character, ALLOCATABLE :: MSPEC(:,:)
      REAL CONC(NBIG)
      INTEGER IS(NBIG),JS(NBIG),KS(NBIG),HRS(NBIG),SPECIES(NBIG)
      REAL BEGTIME, ENDTIME,plon,plat
      REAL XORG,YORG,DX,DY,tlat1,tlat2,rdum
      integer NX, NY, NZ, IDATA, IDATB, idum
      integer MAXS,istag,I,J,K,HR,L,NI


c      open(9,file=ifile,status='old',form='unformatted')
      open(9,file=ifile,status='old',form='unformatted',convert='big_endian')

      READ (9)FILNAM,FILEID,idum,MAXS,IDATA,BEGTIME,IDATB,ENDTIME
      READ (9) plon,plat,idum,XORG,YORG,DX,DY,NX,
     $ NY,NZ,idum,istag,tlat1,tlat2,rdum
      READ  (9) idum,idum, NX,NY

      ALLOCATE(MSPEC(10,MAXS))

      NI=0

      READ  (9) ((MSPEC(I,J),I=1,10),J=1,MAXS)

      DO HR=1,24

       READ(9) idum, rdum, idum, rdum

       DO L=1,MAXS
        DO K=1,NZ

        READ(9) idum, SPECIEN, ((CONC(NI+I+(J-1)*NX),I=1,NX),J=1,NY)

        DO J=1,NY
         DO I=1,NX
         NI=NI+1
         IS(NI)=I
         JS(NI)=J
         KS(NI)=K
         HRS(NI)=HR
         SPECIES(NI)=L
         ENDDO
        ENDDO

        ENDDO
       ENDDO

      ENDDO

      close(9)

      end

      subroutine READ_HEAD(ifile,NX,NY,NZ,MAXS,plon,plat,
     & tlat1,tlat2,XORG,YORG,DX,DY,IDATA,BEGTIME,
     & ENDTIME,MSPEC)

      implicit none

      character*200 ifile
      CHARACTER*4 FILNAM(10),FILEID(60)
      CHARACTER*4 MSPECB(10,200)
      CHARACTER MSPEC(10,200)
      REAL BEGTIME, ENDTIME,plon,plat
      REAL XORG,YORG,DX,DY,tlat1,tlat2,rdum
      integer NX, NY, NZ, IDATA, IDATB, idum
      integer MAXS,istag,I,J


c      open(9,file=ifile,status='old',form='unformatted')
c      open(9,file=ifile,status='old',access='stream',form='unformatted')
      open(9,file=ifile,status='old',form='unformatted',convert='big_endian')


      READ (9)FILNAM,FILEID,idum,MAXS,IDATA,BEGTIME,IDATB,ENDTIME
      READ (9) plon,plat,idum,XORG,YORG,DX,DY,NX,
     $ NY,NZ,idum,istag,tlat1,tlat2,rdum

      READ  (9) idum,idum, NX,NY

      READ  (9) ((MSPECB(I,J),I=1,10),J=1,MAXS)

      DO J=1,MAXS
      DO I=1,10

        READ(MSPECB(I,J),fmt='(a)')MSPEC(I,J)

      ENDDO
      ENDDO

      close(9)

      end

      subroutine READ_HEAD_BIG(ifile,NX,NY,NZ,MAXS,plon,plat,
     & tlat1,tlat2,XORG,YORG,DX,DY,IDATA,BEGTIME,
     & ENDTIME,MSPEC)

      implicit none

      character*200 ifile
      CHARACTER*4 FILNAM(10),FILEID(60)
      CHARACTER*4 MSPECB(10,200)
c      CHARACTER MSPEC(10,200)
      CHARACTER MSPEC(9,200)
      REAL BEGTIME, ENDTIME,plon,plat
      REAL XORG,YORG,DX,DY,tlat1,tlat2,rdum
      integer NX, NY, NZ, IDATA, IDATB, idum
      integer MAXS,istag,I,J


c      open(9,file=ifile,status='old',form='unformatted')
      open(9,file=ifile,status='old',form='unformatted',convert='big_endian')

      READ (9)FILNAM,FILEID,idum,MAXS,IDATA,BEGTIME,IDATB,ENDTIME
      READ (9) plon,plat,idum,XORG,YORG,DX,DY,NX,
     $ NY,NZ,idum,istag,tlat1,tlat2,rdum

      READ  (9) idum,idum, NX,NY

      READ  (9) ((MSPECB(I,J),I=1,10),J=1,MAXS)

      DO J=1,MAXS
c      DO I=1,10
      DO I=1,9

        READ(MSPECB(I,J),fmt='(a)')MSPEC(I,J)

      ENDDO
      ENDDO

      close(9)

      end

      subroutine READ_HEAD_BIG2(ifile,NX,NY,NZ,MAXS,plon,plat,
     & tlat1,tlat2,XORG,YORG,DX,DY,IDATA,BEGTIME,
     & ENDTIME,ifile2)

      implicit none

      character*200 ifile,ifile2
      CHARACTER*4 FILNAM(10),FILEID(60)
      CHARACTER*4 MSPECB(10,200)
      CHARACTER MSPEC(10,200)
      REAL BEGTIME, ENDTIME,plon,plat
      REAL XORG,YORG,DX,DY,tlat1,tlat2,rdum
      integer NX, NY, NZ, IDATA, IDATB, idum
      integer MAXS,istag,I,J


c      open(9,file=ifile,status='old',form='unformatted')
      open(9,file=ifile,status='old',form='unformatted',convert='big_endian')
      open(11,file=ifile2)

      READ (9)FILNAM,FILEID,idum,MAXS,IDATA,BEGTIME,IDATB,ENDTIME
      READ (9) plon,plat,idum,XORG,YORG,DX,DY,NX,
     $ NY,NZ,idum,istag,tlat1,tlat2,rdum

      READ  (9) idum,idum, NX,NY

      READ  (9) ((MSPECB(I,J),I=1,10),J=1,MAXS)

      DO J=1,MAXS
       DO I=1,10
        READ(MSPECB(I,J),fmt='(a)')MSPEC(I,J)
       ENDDO
       WRITE(11,fmt=*)J,",",(MSPEC(I,J),I=1,10)

      ENDDO

      close(9)
      close(11)

      end

      subroutine READ_HEAD_PT(ifile,NSTK,MAXS,plon,plat,
     & tlat1,tlat2,XORG,YORG,DX,DY,IDATA,BEGTIME,
     & ENDTIME,ifile2)

      implicit none

      character*200 ifile,ifile2
      CHARACTER*4 FILNAM(10),FILEID(60)
      CHARACTER*4 MSPECB(10,200)
      CHARACTER MSPEC(10,200)
      REAL BEGTIME, ENDTIME,plon,plat
      REAL XORG,YORG,DX,DY,tlat1,tlat2,rdum
      integer NX, NY, NZ, IDATA, IDATB, idum
      integer MAXS,istag,I,J,NSTK


      open(9,file=ifile,status='old',form='unformatted',convert='big_endian')
c      open(9,file=ifile,status='old',form='unformatted')
      open(11,file=ifile2)

      READ (9)FILNAM,FILEID,idum,MAXS,IDATA,BEGTIME,IDATB,ENDTIME
      READ (9) plon,plat,idum,XORG,YORG,DX,DY,NX,
     $ NY,NZ,idum,istag,tlat1,tlat2,rdum

      READ  (9) idum,idum, NX,NY

      READ  (9) ((MSPECB(I,J),I=1,10),J=1,MAXS)

      READ  (9) idum, NSTK

      DO J=1,MAXS
       DO I=1,10
        READ(MSPECB(I,J),fmt='(a)')MSPEC(I,J)
       ENDDO
       WRITE(11,fmt=*)J,",",(MSPEC(I,J),I=1,10)

      ENDDO

      close(9)
      close(11)

      end

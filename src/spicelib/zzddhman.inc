!
! $ Abstract
!
!     Parameter declarations for the DAF/DAS handle manager.
!
! $ Required_Reading
!
!     DAF, DAS
!
! $ Keywords
!
!     PRIVATE
!
! $ Particulars
!
!     This include file contains parameters defining limits and
!     integer codes that are utilized in the DAF/DAS handle manager
!     routines.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     F.S. Turner       (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 2.6.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    SPICELIB Version 2.5.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 2.4.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 2.3.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 2.2.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 2.1.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-    SPICELIB Version 2.0.0, 12-APR-2012 (BVS)
!
!        Increased FTSIZE (from 1000 to 5000).
!
!-    SPICELIB Version 1.20.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    SPICELIB Version 1.19.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-    SPICELIB Version 1.18.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-    SPICELIB Version 1.17.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    SPICELIB Version 1.16.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    SPICELIB Version 1.15.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.14.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    SPICELIB Version 1.13.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-    SPICELIB Version 1.12.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    SPICELIB Version 1.11.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.10.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    SPICELIB Version 1.9.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    SPICELIB Version 1.8.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    SPICELIB Version 1.7.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-LINUX-64BIT-GCC_C.
!
!-    SPICELIB Version 1.6.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-INTEL_C.
!
!-    SPICELIB Version 1.5.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    SPICELIB Version 1.4.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    SPICELIB Version 1.3.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-    SPICELIB Version 1.2.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN_C.
!
!-    SPICELIB Version 1.1.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    SPICELIB Version 1.0.1, 17-JUL-2002
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 1.0.0, 07-NOV-2001
!
!-&
!
!     Unit and file table size parameters.
!
!     FTSIZE     is the maximum number of files (DAS and DAF) that a
!                user may have open simultaneously.
!
      integer               ftsize
      parameter           ( ftsize = 5000 )

!
!     RSVUNT     is the number of units protected from being locked
!                to a particular handle by ZZDDHHLU.
!
      integer               rsvunt
      parameter           ( rsvunt = 2 )

!
!     SCRUNT     is the number of units protected for use by scratch
!                files.
!
      integer               scrunt
      parameter           ( scrunt = 1 )

!
!     UTSIZE     is the maximum number of logical units this manager
!                will utilize at one time.
!
      integer               utsize
      parameter           ( utsize = 20 + scrunt + rsvunt )

!
!     Access method enumeration.  These parameters are used to
!     identify which access method is associated with a particular
!     handle.  They need to be synchronized with the STRAMH array
!     defined in ZZDDHGSD in the following fashion:
!
!        STRAMH ( READ   ) = 'READ'
!        STRAMH ( WRITE  ) = 'WRITE'
!        STRAMH ( SCRTCH ) = 'SCRATCH'
!        STRAMH ( NEW    ) = 'NEW'
!
!     These values are used in the file table variable FTAMH.
!
      integer               read
      parameter           ( read   = 1 )

      integer               write
      parameter           ( write  = 2 )

      integer               scrtch
      parameter           ( scrtch = 3 )

      integer               new
      parameter           ( new    = 4 )

      integer               numamh
      parameter           ( numamh = 4 )

!
!     Binary file format enumeration.  These parameters are used to
!     identify which binary file format is associated with a
!     particular handle.  They need to be synchronized with the STRBFF
!     array defined in ZZDDHGSD in the following fashion:
!
!        STRBFF ( BIGI3E ) = 'BIG-IEEE'
!        STRBFF ( LTLI3E ) = 'LTL-IEEE'
!        STRBFF ( VAXGFL ) = 'VAX-GFLT'
!        STRBFF ( VAXDFL ) = 'VAX-DFLT'
!
!     These values are used in the file table variable FTBFF.
!
      integer               bigi3e
      parameter           ( bigi3e = 1 )

      integer               ltli3e
      parameter           ( ltli3e = 2 )

      integer               vaxgfl
      parameter           ( vaxgfl = 3 )

      integer               vaxdfl
      parameter           ( vaxdfl = 4 )

      integer               numbff
      parameter           ( numbff = 4 )

!
!     Some random string lengths... more documentation required.
!     For now this will have to suffice.
!

      integer               strsiz
      parameter           ( strsiz = 8 )

      integer               strlen
      parameter           ( strlen = (strsiz+1)*numbff )


!
!     Architecture enumeration.  These parameters are used to identify
!     which file architecture is associated with a particular handle.
!     They need to be synchronized with the STRARC array defined in
!     ZZDDHGSD in the following fashion:
!
!        STRARC ( DAF ) = 'DAF'
!        STRARC ( DAS ) = 'DAS'
!
!     These values will be used in the file table variable FTARC.
!
      integer               daf
      parameter           ( daf = 1 )

      integer               das
      parameter           ( das = 2 )

      integer               numarc
      parameter           ( numarc = 2 )

!
!     For the following environments, record length is measured in
!     characters (bytes) with eight characters per double precision
!     number.
!
!     Environment: Sun, Sun FORTRAN
!     Source:      Sun Fortran Programmer's Guide
!
!     Environment: PC, MS FORTRAN
!     Source:      Microsoft Fortran Optimizing Compiler User's Guide
!
!     Environment: Macintosh, Language Systems FORTRAN
!     Source:      Language Systems FORTRAN Reference Manual,
!                  Version 1.2, page 12-7
!
!     Environment: PC/Linux, g77
!     Source:      Determined by experiment.
!
!     Environment: PC, Lahey F77 EM/32 Version 4.0
!     Source:      Lahey F77 EM/32 Language Reference Manual,
!                  page 144
!
!     Environment: HP-UX 9000/750, FORTRAN/9000 Series 700 computers
!     Source:      FORTRAN/9000 Reference-Series 700 Computers,
!                  page 5-110
!
!     Environment: NeXT Mach OS (Black Hardware),
!                  Absoft Fortran Version 3.2
!     Source:      NAIF Program
!
      integer               recl
      parameter           ( recl  = 1024 )

!
!     The following parameter defines the size of a string used
!     to store a filenames on this target platform.
!
      integer               filen
      parameter           ( filen  =  255 )

!
!     The following parameter controls the size of the character record
!     buffer used to read data from non-native files.
!
      integer               cbfsiz
      parameter           ( cbfsiz = 1024 )

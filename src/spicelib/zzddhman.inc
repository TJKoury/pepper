!
! $ Abstract
!
!     Parameter declarations for the DAF/DAS handle manager.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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
      INTEGER               FTSIZE
      PARAMETER           ( FTSIZE = 5000 )

!
!     RSVUNT     is the number of units protected from being locked
!                to a particular handle by ZZDDHHLU.
!
      INTEGER               RSVUNT
      PARAMETER           ( RSVUNT = 2 )

!
!     SCRUNT     is the number of units protected for use by scratch
!                files.
!
      INTEGER               SCRUNT
      PARAMETER           ( SCRUNT = 1 )

!
!     UTSIZE     is the maximum number of logical units this manager
!                will utilize at one time.
!
      INTEGER               UTSIZE
      PARAMETER           ( UTSIZE = 20 + SCRUNT + RSVUNT )

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
      INTEGER               READ
      PARAMETER           ( READ   = 1 )

      INTEGER               WRITE
      PARAMETER           ( WRITE  = 2 )

      INTEGER               SCRTCH
      PARAMETER           ( SCRTCH = 3 )

      INTEGER               NEW
      PARAMETER           ( NEW    = 4 )

      INTEGER               NUMAMH
      PARAMETER           ( NUMAMH = 4 )

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
      INTEGER               BIGI3E
      PARAMETER           ( BIGI3E = 1 )

      INTEGER               LTLI3E
      PARAMETER           ( LTLI3E = 2 )

      INTEGER               VAXGFL
      PARAMETER           ( VAXGFL = 3 )

      INTEGER               VAXDFL
      PARAMETER           ( VAXDFL = 4 )

      INTEGER               NUMBFF
      PARAMETER           ( NUMBFF = 4 )

!
!     Some random string lengths... more documentation required.
!     For now this will have to suffice.
!

      INTEGER               STRSIZ
      PARAMETER           ( STRSIZ = 8 )

      INTEGER               STRLEN
      PARAMETER           ( STRLEN = (STRSIZ+1)*NUMBFF )


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
      INTEGER               DAF
      PARAMETER           ( DAF = 1 )

      INTEGER               DAS
      PARAMETER           ( DAS = 2 )

      INTEGER               NUMARC
      PARAMETER           ( NUMARC = 2 )

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
      INTEGER               RECL
      PARAMETER           ( RECL  = 1024 )

!
!     The following parameter defines the size of a string used
!     to store a filenames on this target platform.
!
      INTEGER               FILEN
      PARAMETER           ( FILEN  =  255 )

!
!     The following parameter controls the size of the character record
!     buffer used to read data from non-native files.
!
      INTEGER               CBFSIZ
      PARAMETER           ( CBFSIZ = 1024 )

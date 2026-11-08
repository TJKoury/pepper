!$Procedure GETFAT ( Get file architecture and type )

      SUBROUTINE GETFAT ( FILE, ARCH, KERTYP )

! $ Abstract
!
!     Determine the architecture and type of SPICE kernels.
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
!     None.
!
! $ Keywords
!
!     KERNEL
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'zzddhman.inc'

      CHARACTER*(*)         FILE
      CHARACTER*(*)         ARCH
      CHARACTER*(*)         KERTYP

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FILE       I   The name of a file to be examined.
!     ARCH       O   The architecture of the kernel file.
!     KERTYP     O   The type of the kernel file.
!
! $ Detailed_Input
!
!     FILE     is the name of a SPICE kernel file whose architecture
!              and type are desired.
!
! $ Detailed_Output
!
!     ARCH     is the file architecture of the SPICE kernel file
!              specified by FILE. If the architecture cannot be
!              determined or is not recognized the value '?' is
!              returned.
!
!              Architectures currently recognized are:
!
!                 DAF -- The file is based on the DAF architecture.
!                 DAS -- The file is based on the DAS architecture.
!                 XFR -- The file is in a SPICE transfer file format.
!                 DEC -- The file is an old SPICE decimal text file.
!                 ASC -- An ASCII text file.
!                 KPL -- Kernel Pool File (i.e., a text kernel)
!                 TXT -- An ASCII text file.
!                 TE1 -- Text E-Kernel type 1.
!                 ?   -- The architecture could not be determined.
!
!              This variable must be at least 3 characters long.
!
!     KERTYP   is the type of the SPICE kernel file. If the type
!              can not be determined the value '?' is returned.
!
!              Kernel file types may be any sequence of at most four
!              printing characters. NAIF has reserved for its use
!              types which contain all upper case letters.
!
!              A file type of 'PRE' means that the file is a
!              pre-release file.
!
!              This variable may be at most 4 characters long.
!
! $ Parameters
!
!     RECL     is the record length of a binary kernel file. Each
!              record must be large enough to hold 128 double
!              precision numbers. The units in which the record
!              length must be specified vary from environment to
!              environment. For example, VAX Fortran requires
!              record lengths to be specified in longwords,
!              where two longwords equal one double precision
!              number.
!
! $ Exceptions
!
!     1)  If the filename specified is blank, the error
!         SPICE(BLANKFILENAME) is signaled.
!
!     2)  If any inquire on the filename specified by FILE, required to
!         obtain information about the physical file, fails for some
!         reason, the error SPICE(INQUIREERROR) is signaled.
!
!     3)  If the file specified by FILE does not exist, the error
!         SPICE(FILENOTFOUND) is signaled.
!
!     4)  If the file specified by FILE is already open but not through
!         SPICE interfaces, the error SPICE(EXTERNALOPEN) is signaled.
!
!     5)  If an attempt to open the file specified by FILE fails when
!         this routine requires that it succeed, the error
!         SPICE(FILEOPENFAILED) is signaled.
!
!     6)  If an attempt to read the file specified by FILE fails when
!         this routine requires that it succeed, the error
!         SPICE(FILEREADFAILED) is signaled.
!
!     7)  If an issue is detected during the opening the input file or
!         the process to determine its architecture and type, an error
!         is signaled by a routine in the call tree of this routine.
!
!     8)  If the ID word in a DAF based kernel is 'NAIF/DAF', then the
!         algorithm GETFAT uses to distinguish between CK and SPK
!         kernels may result in an indeterminate KERTYP if the SPK or
!         CK files have invalid first segments.
!
! $ Files
!
!     The SPICE kernel file specified by FILE is opened and then
!     closed by this routine to determine its file architecture and
!     type. Filenames of open files should not be passed to this
!     routine.
!
! $ Particulars
!
!     This subroutine is a support utility routine that determines the
!     architecture and type of a SPICE kernel file.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Determine the file architecture and file type of all the
!        kernels loaded through a meta-kernel, and of a kernel in
!        transfer format.
!
!        Use the SPK kernel below to provide an example of a kernel in
!        transfer format.
!
!           earthstns_itrf93_050714.xsp
!
!
!        Use the meta-kernel shown below to load the other types of
!        SPICE kernels.
!
!
!           KPL/MK
!
!           File: getfat_ex1.tm
!
!           This meta-kernel is intended to support operation of SPICE
!           example programs. The kernels shown here should not be
!           assumed to contain adequate or correct versions of data
!           required by SPICE-based user applications.
!
!           In order for an application to use this meta-kernel, the
!           kernels referenced here must be present in the user's
!           current working directory.
!
!           The names and contents of the kernels referenced
!           by this meta-kernel are as follows:
!
!              File name                        Contents
!              ---------                        --------
!              de430.bsp                        Planetary ephemeris
!              mar097.bsp                       Mars satellite ephemeris
!              pck00010.tpc                     Planet orientation and
!                                               radii
!              naif0011.tls                     Leapseconds
!              mgs_moc_v20.ti                   MGS MOC instrument
!                                               parameters
!              mgs_sclkscet_00061.tsc           MGS SCLK coefficients
!              mgs_sc_ext12.bc                  MGS s/c bus attitude
!              mgs_ext12_ipng_mgs95j.bsp        MGS ephemeris
!              megr90n000cb_plate.bds           Plate model based on
!                                               MEGDR DEM, resolution
!                                               4 pixels/degree.
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'de430.bsp',
!                                  'mar097.bsp',
!                                  'pck00010.tpc',
!                                  'naif0011.tls',
!                                  'mgs_moc_v20.ti',
!                                  'mgs_sclkscet_00061.tsc',
!                                  'mgs_sc_ext12.bc',
!                                  'mgs_ext12_ipng_mgs95j.bsp',
!                                  'megr90n000cb_plate.bds'      )
!           \begintext
!
!           End of meta-kernel
!
!
!        Example code begins here.
!
!
!              PROGRAM GETFAT_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               ARCHLN
!              PARAMETER           ( ARCHLN = 4 )
!
!              INTEGER               FILSIZ
!              PARAMETER           ( FILSIZ = 256 )
!
!              INTEGER               KTYPLN
!              PARAMETER           ( KTYPLN = 5 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(ARCHLN)    ARCH
!              CHARACTER*(FILSIZ)    FNAME
!              CHARACTER*(28)        FNAME1
!              CHARACTER*(KTYPLN)    KTYPE
!              CHARACTER*(FILSIZ)    SOURCE
!
!              INTEGER               COUNT
!              INTEGER               HANDLE
!              INTEGER               I
!
!              LOGICAL               FOUND
!
!        C
!        C     Check the file architecture and type of an SPK
!        C     in transfer format.
!        C
!              FNAME1 = 'earthstns_itrf93_050714.xsp'
!
!              CALL GETFAT ( FNAME1, ARCH, KTYPE )
!
!              WRITE(*,*) 'File name     : ', FNAME1
!              WRITE(*,*) '  Architecture: ', ARCH
!              WRITE(*,*) '  Kernel type : ', KTYPE
!              WRITE(*,*) ' '
!
!        C
!        C     Load the kernels.
!        C
!              CALL FURNSH ( 'getfat_ex1.tm' )
!
!        C
!        C     Get the file architecture and kernel type for each of
!        C     the kernels in the kernel pool.
!        C
!              CALL KTOTAL ( 'ALL', COUNT )
!
!              DO I= 1, COUNT
!
!                 CALL KDATA ( I,     'ALL', FNAME, KTYPE, SOURCE,
!             .                HANDLE, FOUND                      )
!
!                 CALL GETFAT ( FNAME, ARCH, KTYPE )
!
!                 WRITE(*,*) 'File name     : ', FNAME
!                 WRITE(*,*) '  Source      : ', SOURCE
!                 WRITE(*,*) '  Architecture: ', ARCH
!                 WRITE(*,*) '  Kernel type : ', KTYPE
!                 WRITE(*,*) ' '
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         File name     : earthstns_itrf93_050714.xsp
!           Architecture: XFR
!           Kernel type : DAF
!
!         File name     : getfat_ex1.tm
!           Source      :
!           Architecture: KPL
!           Kernel type : MK
!
!         File name     : de430.bsp
!           Source      : getfat_ex1.tm
!           Architecture: DAF
!           Kernel type : SPK
!
!         File name     : mar097.bsp
!           Source      : getfat_ex1.tm
!           Architecture: DAF
!           Kernel type : SPK
!
!         File name     : pck00010.tpc
!           Source      : getfat_ex1.tm
!           Architecture: KPL
!           Kernel type : PCK
!
!         File name     : naif0011.tls
!           Source      : getfat_ex1.tm
!           Architecture: KPL
!           Kernel type : LSK
!
!         File name     : mgs_moc_v20.ti
!           Source      : getfat_ex1.tm
!           Architecture: KPL
!           Kernel type : IK
!
!         File name     : mgs_sclkscet_00061.tsc
!           Source      : getfat_ex1.tm
!           Architecture: KPL
!           Kernel type : SCLK
!
!         File name     : mgs_sc_ext12.bc
!           Source      : getfat_ex1.tm
!           Architecture: DAF
!           Kernel type : CK
!
!         File name     : mgs_ext12_ipng_mgs95j.bsp
!           Source      : getfat_ex1.tm
!           Architecture: DAF
!           Kernel type : SPK
!
!         File name     : megr90n000cb_plate.bds
!           Source      : getfat_ex1.tm
!           Architecture: DAS
!           Kernel type : DSK
!
!
! $ Restrictions
!
!     1)  In order to properly determine the type of DAF based binary
!         kernels, the routine requires that their first segments and
!         the meta data necessary to address them are valid.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     H.A. Neilan        (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     F.S. Turner        (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 5.1.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    SPICELIB Version 5.0.1, 07-AUG-2020 (JDR)
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example to $Examples section.
!
!-    SPICELIB Version 5.0.0, 05-FEB-2015 (NJB)
!
!        Updated to support integration of DAS into the
!        handle manager subsystem. Now opened DAS files
!        must be known to that subsystem; if this routine
!        encounters an open, unrecognized DAS file, an
!        error is signaled.
!
!        Corrected various typos in comments.
!
!-    SPICELIB Version 4.25.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 4.24.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 4.23.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 4.22.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 4.21.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-    SPICELIB Version 4.20.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    SPICELIB Version 4.19.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-    SPICELIB Version 4.18.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-    SPICELIB Version 4.17.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    SPICELIB Version 4.16.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    SPICELIB Version 4.15.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 4.14.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    SPICELIB Version 4.13.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-    SPICELIB Version 4.12.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    SPICELIB Version 4.11.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 4.10.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    SPICELIB Version 4.9.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    SPICELIB Version 4.8.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    SPICELIB Version 4.7.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-LINUX-64BIT-GCC_C.
!
!-    SPICELIB Version 4.6.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-INTEL_C.
!
!-    SPICELIB Version 4.5.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    SPICELIB Version 4.4.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    SPICELIB Version 4.3.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-    SPICELIB Version 4.2.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN_C.
!
!-    SPICELIB Version 4.1.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    SPICELIB Version 4.0.2, 24-APR-2003 (EDW)
!
!        Added MAC-OSX-F77 to the list of platforms
!        that require READONLY to read write protected
!        kernels.
!
!-    SPICELIB Version 4.0.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 4.0.0, 22-AUG-2001 (WLT) (FST) (EDW)
!
!        Added code so that the architecture and type of open binary
!        SPICE kernels can be determined.
!
!        Added exception for MACPPC_C (CodeWarrior Mac classic).
!        Reduced RECL value to 12 to prevent expression of
!        the fseek bug.
!
!-    SPICELIB Version 3.2.0, 06-DEC-1999 (WLT)
!
!        The heuristics for distinguishing between CK and SPK have
!        been enhanced so that the routine is no longer requires
!        that TICKS in C-kernels be positive or integral.
!
!-    SPICELIB Version 3.1.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 3.1.3, 22-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 3.1.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 3.1.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 3.1.0, 11-FEB-1999 (FST)
!
!        Added an integrality check to Test 3. If LASTDP is not
!        an integral value, then GETFAT simply returns KERTYP = '?',
!        since it is of an indeterminate type.
!
!-    SPICELIB Version 3.0.0, 07-APR-1998 (NJB)
!
!        Module was updated for the PC-LINUX platform.
!
!-    SPICELIB Version 2.0.0, 19-DEC-1995 (KRG)
!
!        Added several new features to the subroutine:
!
!         - Error handling has been enhanced.
!         - Several new file architectures have been added.
!
!        Removed the mention of 1000 characters as a candidate for the
!        record length of a file.
!
!        Added the exception for a blank filename to the header. The
!        error is signaled, but it was not listed in the header.
!
!        Added IOSTAT values to the appropriate error messages.
!
!        Non-printing characters are replaced with blanks in the ID
!        word when it is read. This deals with the case where a
!        platform allows a text file to be opened as an unformatted
!        file and the ID word does not completely fill 8 characters.
!
!-    SPICELIB Version 1.4.0, 05-JAN-1995 (HAN)
!
!        Removed ENV11 since it is now the same as ENV2.
!        Removed ENV10 since it is the same as the VAX environment.
!
!-    SPICELIB Version 1.3.0, 30-AUG-1994 (HAN)
!
!        Added two new environments, DEC Alpha/OpenVMS and
!        Sun/Solaris, to the source master file.
!
!-    SPICELIB Version 1.2.0, 25-MAR-1994 (HAN)
!
!        Added two new environments, DEC Alpha/OpenVMS and
!        Sun/Solaris, to the source master file.
!
!-    SPICELIB Version 1.1.0, 25-MAR-1994 (HAN)
!
!        Modified master source code file to use READONLY on platforms
!        that support it. Also, changed some local declaration comment
!        lines to match the standard NAIF template.
!
!-    SPICELIB Version 1.0.0, 24-JUL-1993 (WLT) (HAN) (KRG)
!
!-&


! $ Index_Entries
!
!     determine the architecture and type of a kernel file
!
!-&


! $ Revisions
!
!-    SPICELIB Version 4.0.0, 22-AUG-2001 (WLT) (FST) (EDW)
!
!        Added code so that the architecture and type of open binary
!        SPICE kernels can be determined. This uses the new DAF/DAS
!        handle manager as well as examination of handles of open DAS
!        files. Currently the handle manager deals only with DAF
!        files. This routine should be updated again when the DAS
!        system is integrated with the handle manager.
!
!        Some slight changes were required to support ZZDDHFNH on
!        the VAX environment. This resulted in the addition of
!        the logical USEFNH that is set to true in most
!        environments, and never used again other than to allow
!        the invocation of the ZZDDHFNH module.
!
!-    SPICELIB Version 2.0.0, 19-DEC-1995 (KRG)
!
!        Added several new features to the subroutine:
!
!         - Error handling has been enhanced.
!         - Several new file architectures have been added.
!
!        Removed the mention of 1000 characters as a candidate for the
!        record length of a file. It seems unlikely that we will
!        encounter an environment where 1000 characters of storage is
!        larger than the storage necessary for 128 double precision
!        numbers; typically there are 8 characters per double precision
!        number, yielding 1024 characters.
!
!        Added the exception for a blank filename to the header. The
!        error is signaled, but it was not listed in the header.
!
!        Added IOSTAT values to the appropriate error messages.
!
!        Non-printing characters are replaced with blanks in the ID
!        word when it is read. This deals with the case where a
!        platform allows a text file to be opened as an unformatted
!        file and the ID word does not completely fill 8 characters.
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

!
!     Set the length of a SPICE kernel file ID word.
!
      INTEGER               IDLEN
      PARAMETER           ( IDLEN = 12 )

      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

!
!     Set minimum and maximum values for the range of ASCII printing
!     characters.
!
      INTEGER               MINPCH
      PARAMETER           ( MINPCH = 32 )

      INTEGER               MAXPCH
      PARAMETER           ( MAXPCH = 126 )

!
!     Local Variables
!
      CHARACTER*(WDSIZE)    FILARC
      CHARACTER*(FILSIZ)    FNAME
      CHARACTER*(IDLEN)     IDWORD
      CHARACTER*(IDLEN)     TMPWRD

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               INTAMN
      INTEGER               INTARC
      INTEGER               INTBFF
      INTEGER               IOSTAT
      INTEGER               NUMBER

      LOGICAL               DIROPN
      LOGICAL               EXIST
      LOGICAL               FOUND
      LOGICAL               OPENED

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'GETFAT' )
      END IF

!
!     Initialize the temporary storage variables that we use.
!
      IDWORD = ' '

!
!     If the filename we have is blank, signal an error and return.
!
      IF ( FILE .EQ. ' ' ) THEN

         CALL SETMSG ( 'The file name is blank.' )
         CALL SIGERR ( 'SPICE(BLANKFILENAME)'    )
         CALL CHKOUT ( 'GETFAT'                  )
         RETURN

      END IF

!
!     See if this is a binary file that is currently open
!     within the SPICE binary file management subsystem.  At
!     the moment, as far as we know, the file is not opened.
!
      OPENED = .FALSE.

      CALL ZZDDHFNH ( FILE, HANDLE, FOUND )

      IF ( FOUND ) THEN

!
!        If the file was recognized, we need to get the unit number
!        associated with it.
!
         CALL ZZDDHNFO ( HANDLE, FNAME, INTARC, INTBFF, INTAMN, FOUND )

!
!        Translate the architecture ID to a string and retrieve the
!        logical unit to use with this file.
!
         CALL ZZDDHGSD ( 'ARCH', INTARC, FILARC )
         CALL ZZDDHHLU ( HANDLE, FILARC, .FALSE., NUMBER )

         OPENED = .TRUE.

      ELSE

!
!        We'll do a bit of inquiring before we try opening anything.
!
         INQUIRE ( FILE   = FILE, &
                   EXIST  = EXIST, &
                   IOSTAT = IOSTAT, &
                   OPENED = OPENED )

!
!        Not too likely, but if the INQUIRE statement fails...
!
         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'IOSTAT error in INQUIRE statement. IOSTAT ' &
            //              '= #.'                                    )
            CALL ERRINT ( '#', IOSTAT                                 )
            CALL SIGERR ( 'SPICE(INQUIREERROR)'                       )
            CALL CHKOUT ( 'GETFAT'                                    )
            RETURN
         END IF

!
!        Note: the following two tests MUST be performed in the order
!        in which they appear, since in some environments files that do
!        not exist are considered to be open.
!
         IF ( .NOT. EXIST ) THEN

            CALL SETMSG ( 'The kernel file ''#'' does not exist.'   )
            CALL ERRCH  ( '#', FILE                                 )
            CALL SIGERR ( 'SPICE(FILENOTFOUND)'                     )
            CALL CHKOUT ( 'GETFAT'                                  )
            RETURN

         END IF

!
!        Reject open files not known to the handle manager subsystem.
!
         IF ( OPENED ) THEN
!
!           Open files that are not opened within the SPICE
!           binary file management subsystem are forbidden fruit.
!           All we can do is signal an error letting the caller
!           know that we are helpless in this case.
!
            CALL SETMSG ( 'The file ''#'' is already open.' )
            CALL ERRCH  ( '#', FILE                         )
            CALL SIGERR ( 'SPICE(EXTERNALOPEN)'             )
            CALL CHKOUT ( 'GETFAT'                          )
            RETURN

         END IF

      END IF

!
!     Open the file with a record length of RECL (the length of the
!     DAF and DAS records). We assume, for now, that opening the file as
!     a direct access file will work.
!
      DIROPN = .TRUE.

!
!     If the file is not already open (probably the case that
!     happens most frequently) we try opening it for direct access
!     and see if we can locate the idword.
!
      IF ( .NOT. OPENED ) THEN

         CALL GETLUN ( NUMBER )

         OPEN ( UNIT               = NUMBER, &
                FILE               = FILE, &
                ACCESS             = 'DIRECT', &
                RECL               = RECL, &
                STATUS             = 'OLD', &
                IOSTAT             = IOSTAT    )

!
!     If we had trouble opening the file, try opening it as a
!     sequential file.
!
         IF ( IOSTAT .NE. 0 ) THEN

            DIROPN = .FALSE.

            OPEN ( UNIT            = NUMBER, &
                   FILE            = FILE, &
                   ACCESS          = 'SEQUENTIAL', &
                   STATUS          = 'OLD', &
                   IOSTAT          = IOSTAT    )

!
!        If we still have problems opening the file, we don't have a
!        clue about the file architecture and type.
!
            IF ( IOSTAT .NE. 0 ) THEN

               ARCH   = '?'
               KERTYP = '?'
               CALL SETMSG ( 'Attempt to open the file ''#'' failed.' &
               //           ' IOSTAT = #.'                          )
               CALL ERRCH  ( '#', FILE                              )
               CALL ERRINT ( '#', IOSTAT                            )
               CALL SIGERR ( 'SPICE(FILEOPENFAILED)'                )
               CALL CHKOUT ( 'GETFAT'                               )
               RETURN

            END IF

         END IF
      END IF

!
!     We opened the file successfully, so let's try to read from the
!     file. We need to be sure to use the correct form of the read
!     statement, depending on whether the file was opened with direct
!     access or sequential access.
!
      IF ( DIROPN ) THEN

         READ ( NUMBER, REC=1, IOSTAT=IOSTAT ) TMPWRD

!
!        If we couldn't read from the file as a direct access file with
!        a fixed record length, then try to open the file as a
!        sequential file and read from it.
!
         IF ( IOSTAT .NE. 0 ) THEN

            IF ( OPENED ) THEN

!
!              Something has gone wrong here.  The file was opened
!              as either a DAF or DAS prior to the call to GETFAT.
!              We retrieved the unit number maintained by the
!              underlying binary file management system, but we
!              were unable to read the file as direct access.
!              There's nothing we can do but abandon our quest to
!              determine the type of the file.
!
               CALL SETMSG ( 'The file ''#'' is opened as a binary ' &
               //            'SPICE kernel.  But it cannot be ' &
               //            'read using a direct access read. ' &
               //            'The value of IOSTAT returned by the ' &
               //            'attempted READ is #. '                 )
               CALL ERRCH  ( '#', FILE                               )
               CALL ERRINT ( '#', IOSTAT                             )
               CALL SIGERR ( 'SPICE(FILEREADFAILED)'                 )
               CALL CHKOUT ( 'GETFAT'                                )
               RETURN

            END IF

!
!           If we reach this point, the file was opened locally
!           as a direct access file.  We could not read it that
!           way, so we'll try using a sequential read.   However,
!           we first need to close the file and then reopen it
!           for sequential reading.
!
            CLOSE ( NUMBER )

            OPEN ( UNIT   = NUMBER, &
                   FILE   = FILE, &
                   ACCESS = 'SEQUENTIAL', &
                   STATUS = 'OLD', &
                   IOSTAT = IOSTAT    )

!
!           If we could not open the file, we don't have a clue about
!           the file architecture and type.
!
            IF ( IOSTAT .NE. 0 ) THEN

               ARCH   = '?'
               KERTYP = '?'
               CALL SETMSG ( 'Attempt to open the file ''#''' &
               //            ' failed. IOSTAT = #.'           )
               CALL ERRCH  ( '#', FILE                        )
               CALL ERRINT ( '#', IOSTAT                      )
               CALL SIGERR ( 'SPICE(FILEOPENFAILED)'          )
               CALL CHKOUT ( 'GETFAT'                         )
               RETURN

            END IF

!
!           Try to read from the file.
!
            READ ( NUMBER, FMT='(A)', IOSTAT=IOSTAT ) TMPWRD

         END IF

      ELSE

         READ ( NUMBER, FMT='(A)', IOSTAT=IOSTAT ) TMPWRD

      END IF

!
!     If we had an error while reading, we don't recognize this file.
!
      IF ( IOSTAT .NE. 0 ) THEN

         ARCH   = '?'
         KERTYP = '?'
         CLOSE       ( NUMBER                                      )
         CALL SETMSG ( 'Attempt to read from file ''#'' failed.'  // &
                       ' IOSTAT = #.'                              )
         CALL ERRCH  ( '#', FILE                                   )
         CALL ERRINT ( '#', IOSTAT                                 )
         CALL SIGERR ( 'SPICE(FILEREADFAILED)'                     )
         CALL CHKOUT ( 'GETFAT'                                    )
         RETURN

      END IF

!
!     Close the file (if we opened it here), as we do not need it
!     to be open any more.
!
      IF ( .NOT. OPENED ) THEN
         CLOSE ( NUMBER )
      END IF

!
!     At this point, we have a candidate for an ID word. To avoid
!     difficulties with Fortran I/O and other things, we will now
!     replace any non printing ASCII characters with blanks.
!
      DO I = 1, IDLEN
         IF (      ( ICHAR(TMPWRD(I:I)) .LT. MINPCH ) &
              .OR. ( ICHAR(TMPWRD(I:I)) .GT. MAXPCH ) ) THEN
            TMPWRD(I:I) = ' '
         END IF
      END DO

!
!     Identify the architecture and type, if we can.
!
      CALL LJUST  ( TMPWRD, TMPWRD         )
      CALL UCASE  ( TMPWRD, TMPWRD         )
      CALL NEXTWD ( TMPWRD, IDWORD, TMPWRD )

      IF ( IDWORD .EQ. 'DAFETF' ) THEN

!
!        We have a DAF encoded transfer file.
!
         ARCH   = 'XFR'
         KERTYP = 'DAF'

      ELSE IF ( IDWORD .EQ. 'DASETF') THEN

!
!        We have a DAS encoded transfer file.
!
         ARCH   = 'XFR'
         KERTYP = 'DAS'

      ELSE IF ( IDWORD(1:10) .EQ. '''NAIF/DAF''' ) THEN

!
!        We have an old DAF decimal text file.
!
         ARCH   = 'DEC'
         KERTYP = 'DAF'

      ELSE IF ( IDWORD(1:8) .EQ. 'NAIF/DAS' ) THEN

!
!        We have a pre release DAS binary file.
!
         ARCH   = 'DAS'
         KERTYP = 'PRE'

      ELSE

!
!        Get the architecture and type from the ID word, if we can.
!
         CALL IDW2AT ( IDWORD(1:8), ARCH, KERTYP )

      END IF

!
!     If the architecture is DAF and the type is unknown, '?', then we
!     have either an SPK file, a CK file, or something we don't
!     understand. Let's check it out.
!
      IF ( ( ARCH .EQ. 'DAF' ) .AND. ( KERTYP .EQ. '?' ) ) THEN

!
!        We have a DAF file and we do not know what the type is. This
!        situation can occur for older SPK and CK files, before the ID
!        word was used to store type information.
!
!        We use Bill's (WLT'S) magic heuristics to determine the type
!        of the file.
!
!        Open the file and pass the handle to the private routine
!        that deals with the dirty work.
!
         CALL DAFOPR ( FILE, HANDLE )
         CALL ZZCKSPK( HANDLE, KERTYP )
         CALL DAFCLS ( HANDLE )

      END IF

      CALL CHKOUT ( 'GETFAT' )
      RETURN

      END

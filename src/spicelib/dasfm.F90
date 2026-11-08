!$Procedure DASFM ( DAS, file manager )

      subroutine dasfm ( fname, &
                         ftype, &
                         ifname, &
                         handle, &
                         unit, &
                         free, &
                         lastla, &
                         lastrc, &
                         lastwd, &
                         nresvr, &
                         nresvc, &
                         ncomr, &
                         ncomc, &
                         fhset, &
                         access  )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Manage open DAS files.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     DAS
!     FILES
!     UTILITY
!
! $ Declarations

      implicit none

      include 'das.inc'
      include 'errhnd.inc'

      integer(ip)           lbcell
      parameter           ( lbcell  =   -5 )

      character*(*)         fname
      character*(*)         ftype
      character*(*)         ifname
      integer(ip)           handle
      integer(ip)           unit
      integer(ip)           free
      integer(ip)           lastla ( 3 )
      integer(ip)           lastrc ( 3 )
      integer(ip)           lastwd ( 3 )
      integer(ip)           nresvr
      integer(ip)           nresvc
      integer(ip)           ncomr
      integer(ip)           ncomc
      integer(ip)           fhset  ( lbcell : * )
      character*(*)         access

!
!     The record length should be big enough to hold the greatest of the
!     following:
!
!        -- NWD double precision numbers.
!        -- NWI integers.
!        -- NWC characters.
!
!     These parameters are declared in das.inc.
!



!     For the following environments, record length is measured in
!     characters (bytes) with eight characters per double precision
!     number.

!     Environment: Sun, Sun FORTRAN
!     Source:      Sun Fortran Programmer's Guide

!     Environment: PC, MS FORTRAN
!     Source:      Microsoft Fortran Optimizing Compiler User's Guide

!     Environment: Macintosh, Language Systems FORTRAN
!     Source:      Language Systems FORTRAN Reference Manual,
!                  Version 1.2, page 12-7

!     Environment: PC/Linux, Fort77
!     Source:      Determined by experiment.

!     Environment: PC, Lahey F77 EM/32 Version 4.0
!     Source:      Lahey F77 EM/32 Language Reference Manual,
!                  page 144

!     Environment: HP-UX 9000/750, FORTRAN/9000 Series 700 computers
!     Source:      FORTRAN/9000 Reference-Series 700 Computers,
!                  page 5-110

!     Environment: NeXT Mach OS (Black Hardware),
!                  Absoft Fortran Version 3.2
!     Source:      NAIF Program

      integer(ip)           recl
      parameter           ( recl   = 1024 )
      integer(ip)           filen
#if defined(SPICE_OS_WINDOWS) && defined(SPICE_COMPILER_INTEL)
      parameter           ( filen  =  128 )
#else
      parameter           ( filen  =  255 )
#endif



! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     FNAME     I-O  OPR, OPW, ONW, OPN (Obsolete), HFN, FNH
!     FTYPE      I   ONW
!     IFNAME     I   ONW, OPN (Obsolete)
!     HANDLE    I-O  OPR, OPW, ONW, OPN (Obsolete), OPS, LLC, HLU, LUH,
!                    HFN, FNH, HAM, SIH
!     UNIT      I-O  HLU, LUH
!     FREE      I-O  HFS, UFS
!     LASTLA    I-O  HFS, UFS
!     LASTRC    I-O  HFS, UFS
!     LASTWD    I-O  HFS, UFS
!     NRESVR     O   HFS
!     NRESVC     O   HFS
!     NCOMR      O   HFS
!     NCOMC      O   HFS
!     FHSET      O   HOF
!     ACCESS    I-O  SIH, HAM
!     RECL       P   OPR, OPW, ONW, OPN (Obsolete)
!     FTSIZE     P   OPR, OPW, ONW, OPN (Obsolete), LLC, HLU, LUH, HFN,
!                    FNH
!
! $ Detailed_Input
!
!     FNAME    on input is the name of a DAS file to be opened, or
!              the name of a DAS file about which some information
!              (handle, logical unit) is requested.
!
!     FTYPE    on input is a code for the type of data that is
!              contained in the DAS file. This code has no meaning or
!              interpretation at the level of the DAS file
!              architecture, but is provided as a convenience for
!              higher level software. The maximum length for the file
!              type is four (4) characters. If the input string is
!              longer than four characters, the first nonblank
!              character and its three, at most, immediate successors
!              will be used as the file type. The file type may not
!              contain nonprinting characters, and it IS case
!              sensitive.
!
!     IFNAME   is the internal file name for a DAS file to be
!              created.
!
!     HANDLE   on input is the handle of a DAS file about which some
!              information (file name, logical unit) is requested,
!              or the handle of a DAS file to be closed.
!
!     UNIT     on input is the logical unit connected to a DAS file
!              about which some information (file name, handle) is
!              requested.
!
!     FREE     is the Fortran record number of the first free record
!              in a specified DAS file.
!
!     LASTLA   is an array containing the highest current logical
!              addresses, in the specified DAS file, of data of
!              character, double precision, and integer types, in
!              that order.
!
!     LASTRC   is an array containing the Fortran record numbers, in
!              the specified DAS file, of the directory records
!              containing the current last descriptors of clusters
!              of character, double precision, and integer data
!              records, in that order.
!
!     LASTWD   is an array containing the word positions, in the
!              specified DAS file, of the current last descriptors
!              of clusters of character, double precision, and
!              integer data records, in that order.
!
!     ACCESS   is the type of access for which a DAS file is open.
!              The values of ACCESS may be
!
!                 'READ'
!                 'WRITE'
!
!              Leading and trailing blanks are ignored, and case
!              is not significant.
!
!              DAS files that are open for writing may also be read.
!
! $ Detailed_Output
!
!     FNAME    on output is the name of a DAS file for which
!              the corresponding handle or logical unit has been
!              supplied.
!
!     HANDLE   on output is the handle of a DAS file for which
!              the corresponding file name or logical unit has been
!              supplied.
!
!     UNIT     on output is the logical unit connected to a DAS file
!              for which the corresponding file name or handle has
!              been supplied.
!
!     FREE     is the Fortran record number of the first free record
!              in a specified DAS file.
!
!     LASTLA   is an array containing the highest current logical
!              addresses, in the specified DAS file, of data of
!              character, double precision, and integer types, in
!              that order.
!
!     LASTRC   is an array containing the Fortran record numbers, in
!              the specified DAS file, of the directory records
!              containing the current last descriptors of clusters
!              of character, double precision, and integer data
!              records, in that order.
!
!     LASTWD   is an array containing the word positions, in the
!              specified DAS file, of the current last descriptors
!              of clusters of character, double precision, and
!              integer data records, in that order.
!
!     NRESVR   is the number of reserved records in a specified DAS
!              file.
!
!     NRESVC   is the number of characters in use in the reserved
!              record area of a specified DAS file.
!
!     NCOMR    is the number of comment records in a specified DAS
!              file.
!
!     NCOMC    is the number of characters in use in the comment area
!              of a specified DAS file.
!
!     FHSET    is a SPICE set containing the handles of the
!              currently open DAS files.
!
! $ Parameters
!
!     RECL     is the record length of a DAS file. Each record
!              must be large enough to hold the greatest of NWI
!              integers, NWD double precision numbers, or NWC
!              characters, whichever is greater. The units in which
!              the record length must be specified vary from
!              environment to environment. For example, VAX Fortran
!              requires record lengths to be specified in longwords,
!              where two longwords equal one double precision
!              number.
!
!     FTSIZE   is the maximum number of DAS files that a user can
!              have open simultaneously. This includes any files used
!              by the DAS system when closing files opened with write
!              access. Currently, DASCLS (via DASSDR) opens a scratch
!              DAS file using DASOPS to segregate (sort by data
!              type) the records in the DAS file being closed.
!              Segregating the data by type improves the speed of
!              access to the data.
!
!              In order to avoid the possibility of overflowing the
!              DAS file table we recommend, when at least one DAS
!              file is open with write access, that users of this
!              software limit themselves to at most FTSIZE - 2  other
!              open DAS files. If no files are to be open with write
!              access, then users may open FTSIZE files with no
!              possibility of overflowing the DAS file table.
!
! $ Exceptions
!
!     1)  If DASFM is called directly, the error SPICE(BOGUSENTRY)
!         is signaled.
!
!     2)  See entry points DASOPR, DASOPW, DASONW, DASOPN, DASOPS,
!         DASLLC, DASHFS, DASUFS, DASHLU, DASLUH, DASHFN, DASFNH,
!         DASHOF, and DASSIH for exceptions specific to those entry
!         points.
!
! $ Files
!
!     This set of routines is intended to support the creation,
!     updating, and reading of Fortran direct access files that
!     conform to the DAS file format. This format is described in
!     detail in the DAS Required Reading.
!
!     See FTSIZE in the $Parameters section for a description of a
!     potential problem with overflowing the DAS file table when at
!     least one DAS file is opened with write access.
!
! $ Particulars
!
!     As of the N0066 version of the SPICE Toolkit, DASFM uses the
!     SPICE handle manager subsystem and makes use of run-time
!     translation.
!
!     DASFM serves as an umbrella, allowing data to be shared by its
!     entry points:
!
!        DASOPR         Open for read.
!        DASOPW         Open for write.
!        DASONW         Open new.
!        DASOPN         Open new. (Obsolete: Use DASONW instead.)
!        DASOPS         Open as scratch file.
!
!        DASLLC         Low-level close.
!
!        DASHFS         Handle to file summary.
!        DASUFS         Update file summary.
!
!        DASHLU         Handle to logical unit.
!        DASLUH         Logical to handle.
!
!        DASHFN         Handle to name.
!        DASFNH         File name to handle.
!
!        DASHAM         Handle to access method.
!
!        DASHOF         Handles of open files.
!        DASSIH         Signal invalid handles.
!
!
!     Before a DAS file  can be used, it must be opened. Entry points
!     DASOPR and DASOPW provide the only means for opening an
!     existing DAS file.
!
!     Several files may be opened for use simultaneously. (This makes
!     it convenient to combine data from several files to produce a
!     single result, or to route subsets of data from a single source
!     to multiple DAS files.) As each DAS file is opened, it is
!     assigned a file handle, which is used to keep track of the file
!     internally, and which is used by the calling program to refer to
!     the file in all subsequent calls to DAS routines.
!
!     DAS files may be opened for either read or write access. Files
!     open for read access may not be changed in any way. Files opened
!     for write access may be both read from and written to.
!
!     DASONW is used to open a new DAS file. This routine extends the
!     functionality of DASOPN by providing a mechanism for associating a
!     type with the data in the DAS file. The use of this entry over
!     DASOPN is highly recommended.
!
!     Since the only reason for creating a new file is to write
!     something in it, all new files are opened for write access.
!
!     Entry point DASOPN, for opening a new DAS file, has been rendered
!     obsolete by the new entry point DASONW. The entry point DASOPN
!     will continue to be supported for purposes of backward
!     compatibility, but its use in new software development is strongly
!     discouraged.
!
!     Entry point DASOPS creates a new scratch DAS file. As with new
!     permanent files, these files are opened for write access.  DAS
!     files opened by DASOPS are automatically deleted when they are
!     closed.
!
!     Entry point DASLLC is used by DASCLS ( DAS, close file ) to close
!     an open DAS file and update DASFM's bookkeeping information
!     accordingly. DASCLS provides the only official means of closing
!     a DAS file that is currently open. Closing a DAS file any other
!     way (for example, by determining its logical unit and using the
!     Fortran CLOSE statement directly) may affect your calling program
!     in mysterious ways. Normally, DASLLC should not be called by
!     non-SPICELIB routines; these should call DASCLS instead.
!
!     Entry point DASHFS allows you to obtain a file summary for any
!     DAS file that is currently open, without calling DASRFR to
!     re-read the file record. Entry point DASUFS can be used to
!     update a file summary at run-time. Normally, there is no
!     need for routines outside of SPICELIB to modify a DAS file's
!     summary.
!
!     Entry point DASHAM allows you to determine which access method
!     a DAS file has been opened for.
!
!     Entry point DASHOF allows you to determine which DAS files are
!     open at any time. In particular, you can use DASHOF to determine
!     whether any file handle points to an open DAS file.
!
!     Entry point DASSIH signals errors when it is supplied with invalid
!     handles, so it serves to centralize error handling associated
!     with invalid handles.
!
!     The remaining entry points exist mainly to translate between
!     alternative representations of DAS files. There are three ways to
!     identify any open DAS file: by name, by handle, and by logical
!     unit. Given any one of these, you may use these entry points to
!     find the other two.
!
! $ Examples
!
!     See $Examples in entry points DASOPR, DASOPW, DASONW,
!     DASOPN (Obsolete), DASLLC, DASHFS, DASUFS, DASHLU, DASLUH,
!     DASHFN, DASFNH, DASHAM, DASHOF, and DASSIH for examples specific
!     to those entry points.
!
! $ Restrictions
!
!     1)  The value of parameter RECL may need to be changed when DASFM
!         and its entry points are ported to a new environment (CPU and
!         compiler).
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
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 8.1.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    SPICELIB Version 8.0.1, 24-AUG-2021 (JDR)
!
!        Edited the header of the DASFM umbrella routine and all its
!        entry points.
!
!        Removed non-existent argument "SUM" from the umbrella's
!        $Brief_I/O section.
!
!-    SPICELIB Version 8.0.0, 06-APR-2016 (NJB)
!
!        Corrected call to ZZDDHCLS in DASUFS.
!
!        26-FEB-2015 (NJB)
!
!           Now uses DAF/DAS handle manager subsystem.
!
!           FTSIZE is now 5000.
!
!           Corrected miscellaneous spelling errors in comments
!           throughout this file.
!
!-    SPICELIB Version 7.24.0, 10-APR-2014 (NJB)
!
!        Added initializers for file table arrays. This was done
!        to suppress compiler warnings. Deleted declaration of
!        unused parameter BWDLOC.
!
!        Corrected header comments in entry point DASLLC: routine that
!        flushes written, buffered records is DASWBR, not DASWUR.
!
!-    SPICELIB Version 7.23.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 7.22.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 7.21.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 7.20.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 7.19.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-    SPICELIB Version 7.18.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    SPICELIB Version 7.17.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-    SPICELIB Version 7.16.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-    SPICELIB Version 7.15.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    SPICELIB Version 7.14.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    SPICELIB Version 7.13.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 7.12.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    SPICELIB Version 7.11.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-    SPICELIB Version 7.10.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    SPICELIB Version 7.9.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 7.8.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    SPICELIB Version 7.7.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    SPICELIB Version 7.6.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    SPICELIB Version 7.5.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-LINUX-64BIT-GCC_C.
!
!-    SPICELIB Version 7.4.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-INTEL_C.
!
!-    SPICELIB Version 7.3.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    SPICELIB Version 7.2.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    SPICELIB Version 7.1.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-    SPICELIB Version 7.0.0, 28-SEP-2005 (NJB)
!
!        Error handling for non-native files was added to
!        entry points DASOPR and DASOPW.
!
!        Bug in code for constructing long error message in entry
!        point DASUFS was corrected.
!
!-    SPICELIB Version 6.2.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN_C.
!
!-    SPICELIB Version 6.1.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    SPICELIB Version 6.0.3, 24-APR-2003 (EDW)
!
!        Added MAC-OSX-F77 to the list of platforms
!        that require READONLY to read write protected
!        kernels.
!
!-    SPICELIB Version 6.0.2, 21-FEB-2003 (NJB)
!
!        Corrected inline comment in DASLLC: determination of
!        whether file is open is done by searching the handle column of
!        the file table, not the unit column.
!
!-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 6.0.0, 11-DEC-2001 (NJB) (FST)
!
!        To accommodate future updates to the DAS system, including
!        integration with the handle manager and FTP validation
!        checks, the following entry points were modified:
!
!           DASONW, DASOPN
!
!        See their headers and code for the details of the changes.
!
!        Bug fix: removed local buffering of the DAS file ID word
!        and the internal file name, as this was causing DASWFR
!        to exhibit improper behavior.
!
!        Bug fix: missing call to CHKIN was added to an error
!        handling branch in entry point DASUFS. This call is
!        required because DASUFS uses discovery check-in.
!
!-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 5.0.0, 05-APR-1998 (NJB)
!
!        Added references to the PC-LINUX environment. Repaired some
!        format errors involving placement of comment markers in
!        column 1.
!
!-    SPICELIB Version 4.0.1, 19-DEC-1995 (NJB)
!
!        Added permuted index entry section.
!
!-    SPICELIB Version 4.0.0, 31-AUG-1995 (NJB)
!
!        Changed argument list of the entry point DASONW. The input
!        argument NCOMR, which indicates the number of comment records
!        to reserve, was added to the argument list.
!
!-    SPICELIB Version 3.1.0, 05-JAN-1995 (HAN)
!
!        Removed Sun Solaris environment since it is now the same
!        as the Sun OS 4.1.x environment.
!        Removed DEC Alpha/OpenVMS environment since it is now the same
!        as the VAX environment.
!        Entry points affected are: DASFM, DASOPR.
!
!-    SPICELIB Version 3.0.0, 15-JUN-1994 (KRG)
!
!        Modified the umbrella routine DASFM to allow the inclusion of
!        a file type in the creation and manipulation of DAS files.
!
!-    SPICELIB Version 2.0.0, 11-APR-1994 (HAN)
!
!        Updated module to include values for the Silicon Graphics/IRIX,
!        DEC Alpha-OSF/1, and Next/Absoft Fortran platforms. Entry
!        points affected are: DASFM, DASOPR.
!
!-    SPICELIB Version 1.0.0, 15-NOV-1992 (NJB) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     manage open DAS files
!
!-&


! $ Revisions
!
!-    SPICELIB Version 7.0.0, 28-SEP-2005 (NJB)
!
!        Error handling for non-native files was added to
!        entry points DASOPR and DASOPW.
!
!        Bug in code for constructing long error message in entry
!        point DASUFS was corrected.
!
!        Local variable DAS was renamed to DASFIL in DASSIH.
!
!-    SPICELIB Version 6.0.0, 11-DEC-2001 (NJB) (FST)
!
!        Binary File Format Identification:
!
!        The file record now contains an 8 character string that
!        identifies the binary file format utilized by DAS files.
!        The purpose of this string's inclusion in the file record
!        is preparatory in nature, to accelerate the migration to
!        files that support the runtime translation update that
!        is scheduled.
!
!        FTP Validation:
!
!        The file record now contains a sequence of characters
!        commonly corrupted by improper FTP transfers. These
!        characters will be examined by the handle manager when
!        existing files are opened.
!
!        FTIDW and FTIFN have been removed from the elements of
!        the DAS file table. Their presence and use in DASUFS
!        was causing DASWFR difficulties in updating the internal
!        filename under situations where changes to the comment and
!        reserved record parameters in the file record were updated.
!        This change effects DASOPR, DASOPN, DASONW, DASOPW, and
!        DASUFS.
!
!-    SPICELIB Version 3.0.0, 15-JUN-1994 (KRG)
!
!        Modified the umbrella routine DASFM to allow the inclusion of
!        a file type in the creation and manipulation of DAS files. In
!        particular, the following changes were made:
!
!           1) Added variable FTYPE to the SUBROUTINE declaration, and
!              added appropriate entries for this variable in the
!              $Brief_I/O and $Detailed_Input sections of the header.
!
!           2) Removed erroneous references to OPC from the $Brief_I/O
!              section.
!
!           3) Added a new entry point, DASONW, which will support the
!              ability to associate a data type with a new DAS file
!              when it is created. The addition of this new entry point
!              makes the entry point DASOPN obsolete.
!
!           4) Added a description of the new entry point DASONW to the
!              $Particulars section. Also added a statement that the
!              entry point DASOPN has been made obsolete by this new
!              entry point, and its use in new code development is
!              discouraged.
!
!           5) Added a new variable to the file table, FTIDW, which
!              will be used to store the ID words from successfully
!              opened DAS files. We need to maintain this information
!              when writing the file record, as we do not want to
!              modify the ID word in the file.
!
!           6) Removed the parameter DASID as it is no longer needed.
!
!           7) Added new variables TARCH and TTYPE for temporary
!              storage of the file architecture and type. Also added a
!              new variable FNB for storing the position of the first
!              nonblank in a string.
!
!           8) Added new parameters:
!
!                 ARCLEN The maximum length of a file architecture
!                 TYPLEN The maximum length of a file type
!                 MAXPC  Decimal value for the upper limit of printable
!                        ASCII characters.
!                 MINPC  Decimal value for the lower limit of printable
!                        ASCII characters.
!
!           9) Modified entry points which open DAS files: OPR, OPW,
!              OPS, OPN, ONW to support the new file ID word format.
!
!          10) Made all occurrences of error message formatting of
!              filenames consistent. All filenames will be single
!              quoted in output error messages.
!
!          11) Added a test for a blank filename before the inquire
!              to obtain information about a file in the entry points:
!              DASOPR, DASOPW, DASONW, and DASOPN.
!
!          12) Modified the description of FTSIZE in the $Parameters
!              section to reflect the possibility of overflowing the
!              DAS file table when at least one DAS file had been
!              opened with write access.
!
!              The problem occurs when the file table is full, the
!              number of open DAS files equals FTSIZE, and at least one
!              of the open files was opened with write access. If an
!              attempt to close a file opened with write access is made
!              under these conditions, by calling DASCLS, it will fail.
!              DASCLS (via DASSDR) calls DASOPS to open a scratch DAS
!              file, but the scratch file CANNOT be opened because the
!              file table is full. If this occurs, close a file open
!              for read access, or restrict the number of open files
!              in use to be at most FTSIZE - 1 when there will be at
!              least one file opened with write access.
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           lnknfn
      integer(ip)           lnknxt
      integer(ip)           ltrim
      integer(ip)           rtrim

      logical               elemi
      logical               failed
      logical               return

!
!     Local parameters
!
      integer(ip)           acclen
      parameter           ( acclen =   10 )

      integer(ip)           idwlen
      parameter           ( idwlen =    8 )

      integer(ip)           ifnlen
      parameter           ( ifnlen =   60 )

      integer(ip)           lbpool
      parameter           ( lbpool =   -5 )

      integer(ip)           maxpc
      parameter           ( maxpc  =  126 )

      integer(ip)           minpc
      parameter           ( minpc  =   32 )

      integer(ip)           typlen
      parameter           ( typlen =    4 )
!
!     Access method parameters:
!
      integer(ip)           read
      parameter           ( read   =          1 )

      integer(ip)           write
      parameter           ( write  =  read  + 1 )

!
!     File summary parameters:
!
!        A DAS file summary has the following structure:
!
!           +----------------------------------------+
!           | <number of reserved records>           |
!           +----------------------------------------+
!           | <number of characters in r.r. area>    |
!           +----------------------------------------+
!           | <number of comment records>            |
!           +----------------------------------------+
!           | <number of characters in comment area> |
!           +----------------------------------------+
!           | <first free record number>             |
!           +----------------------------------------+
!           | <last character logical address>       |
!           +----------------------------------------+
!           | <last d.p. logical address>            |
!           +----------------------------------------+
!           | <last integer logical address>         |
!           +----------------------------------------+
!           | <last character descriptor record>     |
!           +----------------------------------------+
!           | <last d.p. descriptor record>          |
!           +----------------------------------------+
!           | <last integer descriptor record>       |
!           +----------------------------------------+
!           | <last character descriptor word>       |
!           +----------------------------------------+
!           | <last d.p. descriptor word>            |
!           +----------------------------------------+
!           | <last integer descriptor word>         |
!           +----------------------------------------+
!

      integer(ip)           rrcidx
      parameter           ( rrcidx  =           1 )

      integer(ip)           rchidx
      parameter           ( rchidx  =  rrcidx + 1 )

      integer(ip)           crcidx
      parameter           ( crcidx  =  rchidx + 1 )

      integer(ip)           cchidx
      parameter           ( cchidx  =  crcidx + 1 )

      integer(ip)           freidx
      parameter           ( freidx =   cchidx + 1 )

!
!     Base indices for:
!
!        -- last logical addresses
!        -- records containing last descriptor for a given type
!        -- word containing last descriptor for a given type
!
!     The offset into the file summary for any of these items
!     is obtained by adding the appropriate data type parameter
!     (DP, INT, or CHAR) to the base index for the item.
!
      integer(ip)           llabas
      parameter           ( llabas =   freidx     )

      integer(ip)           lrcbas
      parameter           ( lrcbas =   llabas + 3 )

      integer(ip)           lwdbas
      parameter           ( lwdbas =   lrcbas + 3 )

      integer(ip)           sumsiz
      parameter           ( sumsiz =   lwdbas + 3 )

!
!     Descriptor record pointer locations (within descriptor records):
!
      integer(ip)           fwdloc
      parameter           ( fwdloc =  2 )

!
!     Directory address range location parameters:
!
      integer(ip)           rngbas
      parameter           ( rngbas =   2 )

!
!     First descriptor position in descriptor record:
!
      integer(ip)           begdsc
      parameter           ( begdsc =   9 )

!
!     Length of the Binary File Format string:
!
      integer(ip)           fmtlen
      parameter           ( fmtlen = 8 )

!
!     The parameter TAILEN determines the tail length of a DAS file
!     record.  This is the number of bytes (characters) that
!     occupy the portion of the file record that follows the
!     integer holding the first free address.  For environments
!     with a 32 bit word length, 1 byte characters, and DAS
!     record sizes of 1024 bytes, we have:
!
!           8 bytes - IDWORD
!          60 bytes - IFNAME
!           4 bytes - NRESVR (32 bit integer)
!           4 bytes - NRESVC (32 bit integer)
!           4 bytes - NCOMR  (32 bit integer)
!         + 4 bytes - NCOMC  (32 bit integer)
!          ---------
!          84 bytes - (All file records utilize this space.)
!
!     So the size of the remaining portion (or tail) of the DAS
!     file record for computing environments as described above
!     would be:
!
!        1024 bytes - DAS record size
!      -    8 bytes - DAS Binary File Format Word
!      -   84 bytes - (from above)
!       ------------
!         932 bytes - DAS file record tail length
!
!     Note: environments that do not have a 32 bit word length,
!     1 byte characters, and a DAS record size of 1024 bytes, will
!     require the adjustment of this parameter.
!
      integer(ip)           tailen
      parameter           ( tailen = 932 )

!
!     Local variables
!

!
!     The file table consists of a set of arrays which serve as
!     `columns' of the table.  The sets of elements having the same
!     index in the arrays form the `rows' of the table.  Each column
!     contains a particular type of information; each row contains
!     all of the information pertaining to a particular DAS file.
!
!     All column names in the file table begin with `FT'.  The
!     columns are:
!
!        HAN      Handle
!        ACC      Access method
!        LNK      Number of links
!        SUM      File summary
!
!     The rows of the file table are indexed by a doubly linked
!     list pool.  The pool contains an active list and a free list.
!     when a file is opened, a pointer to the file (the pointer
!     is called a `node').  it is placed at the head of the active
!     list; when a file is closed, the node in the active list that
!     pointed to the file is placed on the free list.
!
!     NEXT is incremented each time a file is opened to become the
!     next file handle assigned.
!
      integer(ip)           fthan  (         ftsize )
      integer(ip)           ftacc  (         ftsize )
      integer(ip)           ftlnk  (         ftsize )
      integer(ip)           ftsum  ( sumsiz, ftsize )

      integer(ip)           pool   ( 2,  lbpool : ftsize )

!
!     FTHEAD is a pointer to the head of the active file list.
!
      integer(ip)           fthead

!
!     NEXT and PREV map the DAS data type codes to their
!     successors and predecessors, respectively.
!
      integer(ip)           next   ( 3 )
      integer(ip)           prev   ( 3 )

!
!     Other local variables
!
      character*(acclen)    acc
      character*(fmtlen)    format
      character*(idwlen)    idword
      character*(filen)     locdas
      character*(ifnlen)    locifn
      character*(fmtlen)    locfmt
      character*(tailen)    tail
      character*(typlen)    ttype

      integer(ip)           curtyp
      integer(ip)           dirrec ( nwi )
      integer(ip)           dsctyp
      integer(ip)           endrec
      integer(ip)           fnb
      integer(ip)           fhlist ( lbcell : ftsize )
      integer(ip)           findex
      integer(ip)           i
      integer(ip)           inqsta
      integer(ip)           iostat
      integer(ip)           last
      integer(ip)           ldrec  ( 3 )
      integer(ip)           ldrmax
      integer(ip)           loc
      integer(ip)           loccch
      integer(ip)           loccrc
      integer(ip)           locrrc
      integer(ip)           locrch
      integer(ip)           maxadr
      integer(ip)           new
      integer(ip)           nrec
      integer(ip)           number
      integer(ip)           nw     ( 3 )
      integer(ip)           nxtdir
      integer(ip)           nxtrec
      integer(ip)           pos
      integer(ip)           prvtyp
      integer(ip)           type

      logical               found
      logical               pass1

!
!     Save everything between calls.
!
      save

!
!     Initial values
!
      data                  ftacc   / ftsize * -1    /
      data                  fthan   / ftsize *  0    /
      data                  ftlnk   / ftsize * -1    /

      data                  pass1   / .true.         /
      data                  fthead  / 0              /

      data                  next    /  2,   3,   1   /
      data                  prev    /  3,   1,   2   /
      data                  nw      /  nwc, nwd, nwi /

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin  ( 'DASFM' )
         call sigerr ( 'SPICE(BOGUSENTRY)' )
         call chkout ( 'DASFM' )
      end if

      return




!$Procedure DASOPR ( DAS, open for read )

      entry dasopr ( fname, handle )

! $ Abstract
!
!     Open a DAS file for reading.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     DAS
!     FILES
!
! $ Declarations
!
!     CHARACTER*(*)         FNAME
!     INTEGER               HANDLE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FNAME      I   Name of a DAS file to be opened.
!     HANDLE     O   Handle assigned to the opened DAS file.
!
! $ Detailed_Input
!
!     FNAME    is the name of a DAS file to be opened with read
!              access.
!
! $ Detailed_Output
!
!     HANDLE   is the handle that is  associated with the file. This
!              handle is used to identify the file in subsequent
!              calls to other DAS routines.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input filename is blank, the error
!         SPICE(BLANKFILENAME) is signaled.
!
!     2)  If the specified file does not exist, the error
!         SPICE(FILENOTFOUND) is signaled.
!
!     3)  If the specified file has already been opened for read
!         access, the handle already associated with the file is
!         returned.
!
!     4)  If the specified file has already been opened for write
!         access, the error SPICE(DASRWCONFLICT) is signaled.
!
!     5)  If the specified file has already been opened by a non-DAS
!         routine, the error SPICE(DASIMPROPOPEN) is signaled.
!
!     6)  If the specified file cannot be opened without exceeding
!         the maximum allowed number of open DAS files, the error
!         SPICE(DASFTFULL) is signaled.
!
!     7)  If the named file cannot be opened properly, an error is
!         signaled by a routine in the call tree of this routine.
!
!     8)  If the file record cannot be read, the error
!         SPICE(FILEREADFAILED) is signaled.
!
!     9)  If the specified file is not a DAS file, as indicated by the
!         file's ID word, an error is signaled by a routine in the call
!         tree of this routine.
!
!     10) If no logical units are available, an error is signaled
!         by a routine in the call tree of this routine.
!
! $ Files
!
!     See argument FNAME.
!
! $ Particulars
!
!     Most DAS files require only read access. If you do not need to
!     change the contents of a file, you should open it using DASOPR.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Dump several parameters from the first DLA segment of a DSK
!        file. Note that DSK files are based on DAS. The segment is
!        assumed to be of type 2.
!
!        Example code begins here.
!
!
!              PROGRAM DASOPR_EX1
!              IMPLICIT NONE
!
!              INCLUDE 'dla.inc'
!              INCLUDE 'dskdsc.inc'
!              INCLUDE 'dsk02.inc'
!
!        C
!        C     Local parameters
!        C
!              INTEGER               FILSIZ
!              PARAMETER           ( FILSIZ = 255 )
!
!              INTEGER               LNSIZE
!              PARAMETER           ( LNSIZE = 80 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(FILSIZ)    DSK
!              CHARACTER*(LNSIZE)    OUTLIN
!
!              DOUBLE PRECISION      VOXORI ( 3 )
!              DOUBLE PRECISION      VOXSIZ
!              DOUBLE PRECISION      VTXBDS ( 2, 3 )
!
!              INTEGER               CGSCAL
!              INTEGER               DLADSC ( DLADSZ )
!              INTEGER               HANDLE
!              INTEGER               NP
!              INTEGER               NV
!              INTEGER               NVXTOT
!              INTEGER               VGREXT ( 3 )
!              INTEGER               VOXNPL
!              INTEGER               VOXNPT
!              INTEGER               VTXNPL
!
!              LOGICAL               FOUND
!
!
!        C
!        C     Prompt for the name of the DSK to read.
!        C
!              CALL PROMPT ( 'Enter DSK name > ', DSK )
!        C
!        C     Open the DSK file for read access.
!        C     We use the DAS-level interface for
!        C     this function.
!        C
!              CALL DASOPR ( DSK, HANDLE )
!
!        C
!        C     Begin a forward search through the
!        C     kernel, treating the file as a DLA.
!        C     In this example, it's a very short
!        C     search.
!        C
!              CALL DLABFS ( HANDLE, DLADSC, FOUND )
!
!              IF ( .NOT. FOUND ) THEN
!        C
!        C        We arrive here only if the kernel
!        C        contains no segments.  This is
!        C        unexpected, but we're prepared for it.
!        C
!                 CALL SETMSG ( 'No segments found '
!             .   //            'in DSK file #.'    )
!                 CALL ERRCH  ( '#',  DSK           )
!                 CALL SIGERR ( 'SPICE(NODATA)'     )
!
!              END IF
!
!        C
!        C     If we made it this far, DLADSC is the
!        C     DLA descriptor of the first segment.
!        C
!        C     Read and display type 2 bookkeeping data.
!        C
!              CALL DSKB02 ( HANDLE, DLADSC, NV,     NP,     NVXTOT,
!             .              VTXBDS, VOXSIZ, VOXORI, VGREXT, CGSCAL,
!             .              VTXNPL, VOXNPT, VOXNPL                 )
!
!        C
!        C     Show vertex and plate counts.
!        C
!              OUTLIN = 'Number of vertices:                 #'
!              CALL REPMI  ( OUTLIN, '#', NV, OUTLIN )
!              CALL TOSTDO ( OUTLIN )
!
!              OUTLIN = 'Number of plates:                   #'
!              CALL REPMI  ( OUTLIN, '#', NP, OUTLIN )
!              CALL TOSTDO ( OUTLIN )
!
!              OUTLIN = 'Voxel edge length (km):             #'
!              CALL REPMF  ( OUTLIN, '#', VOXSIZ, 6, 'E', OUTLIN )
!              CALL TOSTDO ( OUTLIN )
!
!              OUTLIN = 'Number of voxels:                   #'
!              CALL REPMI  ( OUTLIN, '#', NVXTOT, OUTLIN )
!              CALL TOSTDO ( OUTLIN )
!
!        C
!        C     Close the kernel.  This isn't necessary in a stand-
!        C     alone program, but it's good practice in subroutines
!        C     because it frees program and system resources.
!        C
!              CALL DASCLS ( HANDLE )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, using the DSK file named phobos512.bds, the output
!        was:
!
!
!        Enter DSK name > phobos512.bds
!        Number of vertices:                 1579014
!        Number of plates:                   3145728
!        Voxel edge length (km):             1.04248E-01
!        Number of voxels:                   11914500
!
!
! $ Restrictions
!
!     None.
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
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 8.0.1, 19-JUL-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!        Added code example using example from DSKB02.
!
!        Updated $Exceptions entries #7 and #9: error handling for the
!        DAS open failure and "file is not a DAS file" cases is now
!        performed by lower level code (ZZDDHOPN).
!
!-    SPICELIB Version 8.0.0, 30-JUL-2014 (NJB)
!
!        Now uses DAF/DAS handle manager subsystem.
!
!-    SPICELIB Version 7.0.0, 28-SEP-2005 (NJB)
!
!        Error handling for non-native files was added.
!
!-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 6.0.0, 14-DEC-2001 (FST)
!
!        The DAS file ID word and internal file name are no longer
!        buffered by this routine. See DASFM's $Revisions section
!        for details.
!
!-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 3.0.0, 15-JUN-1994 (KRG)
!
!        Modified the entry point to use the new file ID format which
!        contains a mnemonic code for the data type. Added error
!        checks on file names. Fixed bug involving use of sign of
!        file handles. Improved some error messages. (delete rest)
!
!-    SPICELIB Version 2.0.0, 11-APR-1994 (HAN)
!
!        Updated module to include values for the Silicon Graphics/IRIX,
!        DEC Alpha-OSF/1, and Next/Absoft Fortran platforms. Entry
!        points affected are: DASFM, DASOPR.
!
!-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     open a DAS file for reading
!     open a DAS file for read access
!
!-&


! $ Revisions
!
!-    SPICELIB Version 8.0.0, 30-JUL-2014 (NJB)
!
!        Now uses DAF/DAS handle manager subsystem.
!
!-    SPICELIB Version 7.0.0, 28-SEP-2005 (NJB)
!
!        Error handling for non-native files was added.
!
!-    SPICELIB Version 3.0.1, 24-APR-2003 (EDW)
!
!        Added MAC-OSX-F77 to the list of platforms
!        that require READONLY to read write protected
!        kernels.
!
!-    SPICELIB Version 3.0.0, 15-JUN-1994 (KRG)
!
!        Modified the entry point to use the new file ID format which
!        contains a mnemonic code for the data type.
!
!        Split an IF ... ELSE IF ... statement into 2 IF statements of
!        equivalent behavior to allow testing of the file architecture.
!
!        Added code to test the file architecture and to verify that the
!        file is a DAS file.
!
!        Removed the error SPICE(DASNOIDWORD) as it was no longer
!        relevant.
!
!        Added the error SPICE(NOTADASFILE) if this routine is called
!        with a file that does not contain an ID word identifying the
!        file as a DAS file.
!
!        Added a test for a blank filename before attempting to use the
!        filename in the routine. If the filename is blank, the error
!        SPICE(BLANKFILENAME) will be signaled.
!
!        Fixed a bug when dealing with a read/write open conflict for
!        DAS files: the code used the DAF positive/negative handle
!        method to determine read/write access rather than the DAS file
!        table column FTACC. Replaced the code:
!
!           IF ( FTHAN(FINDEX) .LT. 0 ) THEN
!
!        with
!
!           IF ( FTACC(FINDEX) .EQ. WRITE ) THEN
!
!        Changed the long error message when the error
!        SPICE(NOTADASFILE) is signaled to suggest that a common error
!        is attempting to use a text version of the desired file rather
!        than the binary version.
!
!-    SPICELIB Version 2.0.0, 11-APR-1994 (HAN)
!
!        Updated module to include values for the Silicon Graphics/IRIX,
!        DEC Alpha-OSF/1, and Next/Absoft Fortran platforms. Entry
!        points affected are: DASFM, DASOPR.
!
!-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'DASOPR' )

!
!     Initialize the file table pool and handle list, if necessary.
!
      if ( pass1 ) then

         call lnkini ( ftsize, pool   )
         call ssizei ( ftsize, fhlist )

         pass1 = .false.

      end if

!
!     Try to open the file for READ access.
!
!
!        The file may or may not already be open. If so, it should have
!        not been opened for writing (FTACC .EQ. WRITE). If opened for
!        reading, ZZDDHOPN will just increment the number of links and
!        return the handle.
!
      call zzddhopn ( fname, 'READ', 'DAS', handle )

      if ( failed() ) then
         call chkout ( 'DASOPR' )
         return
      end if

!
!     Determine whether this file is already in the file table.
!     Search for a matching handle.
!
      findex = fthead
      found  = .false.

      do while (  ( .not. found )  .and.  ( findex .gt. 0 )  )

         if ( fthan(findex) .eq. handle ) then
            found = .true.
         else
            findex = lnknxt ( findex, pool )
         end if

      end do

      if ( found ) then
!
!        The file is already open for read access. Increment the number
!        of links to this file.
!
         ftlnk(findex) = ftlnk(findex) + 1
         handle        = fthan(findex)
!
!        There's nothing else to do.
!
         call chkout ( 'DASOPR' )
         return

      end if

!
!     This file has no entry in the file table. We need to
!     create a new entry. See whether there's room for it.
!
      if ( lnknfn(pool) .eq. 0  ) then

         call setmsg ( 'The file table is full, with # entries. ' &
         //            'Could not open ''#''.'                    )
         call errint ( '#', ftsize                                )
         call errch  ( '#', fname                                 )
         call sigerr ( 'SPICE(DASFTFULL)'                         )
         call chkout ( 'DASOPR'                                   )
         return

      end if

!
!     Read the file's file record.
!
      call zzdasrfr ( handle, idword, locifn, locrrc, &
                      locrch, loccrc, loccch         )

      if ( failed() ) then
         call chkout ( 'DASOPR' )
         return
      end if

!
!     Update the file table to include information about our newly
!     opened DAS file. Link the information for this file at the
!     head of the file table list.
!
      call lnkan  ( pool, new          )
      call lnkilb ( new,  fthead, pool )

      fthead           =   new
      fthan (fthead)   =   handle
      ftacc (fthead)   =   read
      ftlnk (fthead)   =   1

!
!     Fill in the file summary. We already know how many reserved
!     records and comment records there are. To find the number of the
!     first free record, the last logical address of each type, and the
!     locations of the last descriptors of each type, we must examine
!     the directory records. We do not assume that the data records in
!     the DAS file have been segregated.
!
      call cleari ( sumsiz, ftsum (1,fthead) )

      ftsum ( rrcidx, fthead )  =  locrrc
      ftsum ( rchidx, fthead )  =  locrch
      ftsum ( crcidx, fthead )  =  loccrc
      ftsum ( cchidx, fthead )  =  loccch

!
!     We'll find the values for each data type separately.
!
      do type = 1, 3
!
!        The first directory record is located right after the
!        last comment record.
!
         nrec  =  locrrc + loccrc + 2

!
!        Keep track of the record number of the last data
!        record of the current type.
!
         ldrec(type)  =  0

!
!        Find the last directory containing a descriptor of a
!        record cluster of the current type.
!
         call zzdasgri( handle, nrec, dirrec )

         maxadr  =  dirrec ( rngbas  +  2 * type )
         nxtdir  =  dirrec ( fwdloc              )


         do while ( nxtdir .gt. 0 )
!
!           Read the directory record. If this record contains
!           descriptors for clusters we're interested in, update the
!           directory record number.
!
            call zzdasgri ( handle, nxtdir, dirrec )

            if ( failed() ) then
               call chkout ( 'DASOPR' )
               return
            end if


            if (  dirrec(rngbas + 2*type)  .gt.  0  ) then

               maxadr  =  dirrec ( rngbas  +  2 * type  )
               nrec    =  nxtdir

            end if

            nxtdir = dirrec ( fwdloc )

         end do

!
!        At this point, NREC is the record number of the directory
!        containing the last descriptor for clusters of TYPE, if
!        there are any such descriptors.
!
!        MAXADR is the maximum logical address of TYPE.
!
         ftsum ( llabas + type,  fthead )  =  maxadr

         if ( maxadr .gt. 0 ) then
            ftsum ( lrcbas + type,  fthead )  =  nrec
         else
            ftsum ( lrcbas + type,  fthead )  =  0
         end if
!
!        We still need to set the word location of the final
!        descriptor of TYPE, if there are any descriptors of TYPE.
!
         if ( maxadr .gt. 0 ) then
!
!           Re-read the directory record containing the last descriptor
!           of the current type.
!
            call zzdasgri ( handle, nrec, dirrec )

            if ( failed() ) then
               call chkout ( 'DASOPR' )
               return
            end if

!
!           Traverse the directory record, looking for the last
!           descriptor of TYPE. We'll keep track of the maximum logical
!           address of TYPE for each cluster of TYPE whose descriptor
!           we examine. When this value is the maximum logical address
!           of TYPE, we've found the last descriptor of TYPE.
!
!           Also keep track of the end record numbers for each
!           cluster.
!
            last    =  dirrec( rngbas  +  (2*type - 1)  )  -  1
            dsctyp  =  dirrec( begdsc                   )
            prvtyp  =  prev  ( dsctyp                   )
            endrec  =  nrec
            pos     =  begdsc

            do while ( last .lt. maxadr )

               pos = pos + 1

               if ( dirrec(pos) .gt. 0 ) then
                  curtyp = next(prvtyp)
               else
                  curtyp = prev(prvtyp)
               end if

               if ( curtyp .eq. type ) then
                  last = last  +  nw(type) * abs( dirrec(pos) )
               end if

               endrec = endrec + abs( dirrec(pos) )
               prvtyp = curtyp

            end do
!
!           At this point, POS is the word position of the last
!           descriptor of TYPE, and ENDREC is the record number
!           of the last data record of TYPE.
!
            ftsum ( lwdbas + type, fthead )  =  pos
            ldrec ( type )                   =  endrec


         else
!
!           There's no data of TYPE in the file.
!
            ftsum ( lwdbas + type, fthead )  =  0
            ldrec ( type )                   =  0

         end if

      end do

!
!     We're almost done; we need to find the number of the first free
!     record. This record follows all of the data records and all of
!     the directory records. It may happen that the last record in use
!     is an empty directory.
!
      call maxai ( ldrec, 3, ldrmax, loc )

      nrec   = locrrc + loccrc + 2

      call zzdasgri ( handle,  nrec,  dirrec  )

      nxtrec = dirrec(fwdloc)


      do while ( nxtrec .ne. 0 )

         nrec   = nxtrec

         call zzdasgri ( handle,  nrec,  dirrec  )

         nxtrec = dirrec(fwdloc)

      end do

!
!     Now NREC is the last directory record.
!
      ftsum ( freidx, fthead)   =   max ( ldrmax, nrec )  +  1

!
!     Insert the new handle into our handle set.
!
      call insrti ( handle, fhlist )

      call chkout ( 'DASOPR' )
      return




!$Procedure DASOPW ( DAS, open for write )

      entry dasopw ( fname, handle )

! $ Abstract
!
!     Open a DAS file for writing.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     DAS
!     FILES
!
! $ Declarations
!
!     CHARACTER*(*)         FNAME
!     INTEGER               HANDLE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FNAME      I   Name of a DAS file to be opened.
!     HANDLE     O   Handle assigned to the opened DAS file.
!
! $ Detailed_Input
!
!     FNAME    is the name of a DAS file to be opened with write
!              access.
!
! $ Detailed_Output
!
!     HANDLE   is the handle that is associated with the file. This
!              handle is used to identify the file in subsequent
!              calls to other DAS routines.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input filename is blank, an error is signaled by a
!         routine in the call tree of this routine.
!
!     2)  If the specified file does not exist, an error is signaled by
!         a routine in the call tree of this routine.
!
!     3)  If the specified file has already been opened, either by the
!         DAS file routines or by other code, an error is signaled by a
!         routine in the call tree of this routine. Note that this
!         response is not paralleled by DASOPR, which allows you to open
!         a DAS file for reading even if it is already open for reading.
!
!     4)  If the specified file cannot be opened without exceeding
!         the maximum allowed number of open DAS files, the error
!         SPICE(DASFTFULL) is signaled.
!
!     5)  If the specified file cannot be opened properly, an error
!         is signaled by a routine in the call tree of this routine.
!
!     6)  If the file record cannot be read, an error is signaled by a
!         routine in the call tree of this routine.
!
!     7)  If the specified file is not a DAS file, as indicated by the
!         file's ID word, an error is signaled by a routine in the call
!         tree of this routine.
!
!     8)  If no logical units are available, an error is signaled
!         by a routine in the call tree of this routine.
!
! $ Files
!
!     See argument FNAME.
!
! $ Particulars
!
!     Most DAS files require only read access. If you do not need to
!     change the contents of a file, you should open it with DASOPR.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Create a new DAS file containing 200 integer addresses set
!        to zero. Re-open the file for write access again, and write
!        to its addresses 1 through 200 in random-access fashion by
!        updating the file.
!
!
!        Example code begins here.
!
!
!              PROGRAM DASOPW_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local Parameters
!        C
!              CHARACTER*(*)         DASNAM
!              PARAMETER           ( DASNAM   = 'dasopw_ex1.das' )
!
!              INTEGER               IDATLN
!              PARAMETER           ( IDATLN = 200 )
!
!              INTEGER               TYPELN
!              PARAMETER           ( TYPELN = 4   )
!
!        C
!        C     Local Variables
!        C
!              CHARACTER*(TYPELN)    TYPE
!
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               IDATA   ( IDATLN )
!              INTEGER               J
!
!        C
!        C     Open a new DAS file. Reserve no comment records.
!        C
!              TYPE = 'TEST'
!              CALL DASONW ( DASNAM, TYPE,  'TEST/DASOPW_EX1',
!             .              0,      HANDLE                  )
!
!        C
!        C     Append 200 integers to the file; after the data are
!        C     present, we're free to update it in any order we
!        C     please. (CLEARI zeros out an integer array.)
!        C
!              CALL CLEARI (         IDATLN, IDATA )
!              CALL DASADI ( HANDLE, IDATLN, IDATA )
!
!        C
!        C     Close the file.
!        C
!              CALL DASCLS ( HANDLE )
!
!        C
!        C     Open the file again for writing.
!        C
!              CALL DASOPW ( DASNAM, HANDLE )
!
!        C
!        C     Reset the data array, and read the data into it.
!        C
!              CALL FILLI  ( -1, IDATLN, IDATA )
!              CALL DASRDI ( HANDLE,  1, IDATLN, IDATA )
!
!        C
!        C     Print the contents of the file before updating it.
!        C
!              WRITE(*,*) 'Contents of ' // DASNAM // ' before update:'
!              WRITE(*,*) ' '
!              DO I = 1, 20
!                 WRITE (*,'(10I5)') (IDATA((I-1)*10+J), J = 1, 10)
!              END DO
!
!        C
!        C     Now the integer logical addresses 1:200 can be
!        C     written to in random-access fashion. We'll fill them
!        C     in reverse order.
!        C
!              DO I = IDATLN, 1, -1
!                 CALL DASUDI ( HANDLE, I, I, I )
!              END DO
!
!        C
!        C     Close the file.
!        C
!              CALL DASCLS ( HANDLE )
!
!        C
!        C     Now make sure that we updated the file properly.
!        C     Open the file for reading and dump the contents
!        C     of the integer logical addresses 1:200.
!        C
!              CALL DASOPR ( DASNAM, HANDLE )
!
!              CALL CLEARI (             IDATLN, IDATA )
!              CALL DASRDI ( HANDLE,  1, IDATLN, IDATA )
!
!              WRITE(*,*) ' '
!              WRITE(*,*) 'Contents of ' // DASNAM // ' after update:'
!              WRITE(*,*) ' '
!              DO I = 1, 20
!                 WRITE (*,'(10I5)') (IDATA((I-1)*10+J), J = 1, 10)
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Contents of dasopw_ex1.das before update:
!
!            0    0    0    0    0    0    0    0    0    0
!            0    0    0    0    0    0    0    0    0    0
!            0    0    0    0    0    0    0    0    0    0
!            0    0    0    0    0    0    0    0    0    0
!            0    0    0    0    0    0    0    0    0    0
!            0    0    0    0    0    0    0    0    0    0
!            0    0    0    0    0    0    0    0    0    0
!            0    0    0    0    0    0    0    0    0    0
!            0    0    0    0    0    0    0    0    0    0
!            0    0    0    0    0    0    0    0    0    0
!            0    0    0    0    0    0    0    0    0    0
!            0    0    0    0    0    0    0    0    0    0
!            0    0    0    0    0    0    0    0    0    0
!            0    0    0    0    0    0    0    0    0    0
!            0    0    0    0    0    0    0    0    0    0
!            0    0    0    0    0    0    0    0    0    0
!            0    0    0    0    0    0    0    0    0    0
!            0    0    0    0    0    0    0    0    0    0
!            0    0    0    0    0    0    0    0    0    0
!            0    0    0    0    0    0    0    0    0    0
!
!         Contents of dasopw_ex1.das after update:
!
!            1    2    3    4    5    6    7    8    9   10
!           11   12   13   14   15   16   17   18   19   20
!           21   22   23   24   25   26   27   28   29   30
!           31   32   33   34   35   36   37   38   39   40
!           41   42   43   44   45   46   47   48   49   50
!           51   52   53   54   55   56   57   58   59   60
!           61   62   63   64   65   66   67   68   69   70
!           71   72   73   74   75   76   77   78   79   80
!           81   82   83   84   85   86   87   88   89   90
!           91   92   93   94   95   96   97   98   99  100
!          101  102  103  104  105  106  107  108  109  110
!          111  112  113  114  115  116  117  118  119  120
!          121  122  123  124  125  126  127  128  129  130
!          131  132  133  134  135  136  137  138  139  140
!          141  142  143  144  145  146  147  148  149  150
!          151  152  153  154  155  156  157  158  159  160
!          161  162  163  164  165  166  167  168  169  170
!          171  172  173  174  175  176  177  178  179  180
!          181  182  183  184  185  186  187  188  189  190
!          191  192  193  194  195  196  197  198  199  200
!
!
!        Note that after run completion, a new DAS file exists in the
!        output directory.
!
! $ Restrictions
!
!     None.
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     F.S. Turner        (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 8.0.1, 19-JUL-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example.
!
!        Updated $Exceptions #3 -- short error message is
!        SPICE(FILEOPENCONFLICT) -- #5 and #7 -- error handling for
!        the DAS open failure and "file is not a DAS file" cases is
!        now performed by lower level code (ZZDDHOPN).
!
!-    SPICELIB Version 8.0.0, 30-JUL-2014 (NJB)
!
!        Now uses DAF/DAS handle manager subsystem.
!
!-    SPICELIB Version 7.0.0, 28-SEP-2005 (NJB)
!
!        Error handling for non-native files was added.
!
!-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 6.0.0, 14-DEC-2001 (FST)
!
!        The DAS file ID word and internal file name are no longer
!        buffered by this routine. See DASFM's $Revisions section
!        for details.
!
!-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 2.0.0, 29-OCT-1993 (KRG)
!
!        Modified the entry point to use the new file ID format which
!        contains a mnemonic code for the data type.
!
!-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     open a DAS file for writing
!     open a DAS file for write access
!
!-&


! $ Revisions
!
!-    SPICELIB Version 8.0.0, 30-JUL-2014 (NJB)
!
!        Now uses DAF/DAS handle manager subsystem.
!
!-    SPICELIB Version 7.0.0, 28-SEP-2005 (NJB)
!
!        Error handling for non-native files was added.
!
!-    SPICELIB Version 2.0.0, 29-OCT-1993 (KRG)
!
!        Modified the entry point to use the new file ID format which
!        contains a mnemonic code for the data type.
!
!        Split an IF ... ELSE IF ... statement into 2 IF statements of
!        equivalent behavior to allow testing of the file architecture.
!
!        Added code to test the file architecture and to verify that the
!        file is a DAS file.
!
!        Removed the error SPICE(DASNOIDWORD) as it was no longer
!        relevant.
!
!        Added the error SPICE(NOTADASFILE) if this routine is called
!        with a file that does not contain an ID word identifying the
!        file as a DAF file.
!
!        Added a test for a blank filename before attempting to use the
!        filename in the routine. If the filename is blank, the error
!        SPICE(BLANKFILENAME) will be signaled.
!
!        Changed the long error message when the error
!        SPICE(NOTADASFILE) is signaled to suggest that a common error
!        is attempting to load a text version of the desired file rather
!        than the binary version.
!
!-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'DASOPW' )

!
!     Initialize the file table pool and handle list, if necessary.
!
      if ( pass1 ) then

         call lnkini ( ftsize, pool   )
         call ssizei ( ftsize, fhlist )

         pass1 = .false.

      end if


      if ( lnknfn(pool) .eq. 0  ) then

         call setmsg ( 'The file table is full, with # entries. '     // &
                       'Could not open ''#''.'                         )
         call errint ( '#', ftsize                                     )
         call errch  ( '#', fname                                      )
         call sigerr ( 'SPICE(DASFTFULL)'                              )
         call chkout ( 'DASOPW'                                        )
         return

      end if

!
!     Open the file for writing:   open the file, get the
!     internal file name, and set the number of links to one.
!
      call zzddhopn ( fname, 'WRITE', 'DAS', handle )

!
!     Read the file record.
!
      call zzdasrfr ( handle, idword, locifn, locrrc, &
                      locrch, loccrc, loccch         )

      if ( failed() ) then
         call chkout ( 'DASOPW' )
         return
      end if

!
!     At this point, we know that we have a valid DAS file, and
!     we're set up to read from it, so ...
!
!     Update the file table to include information about
!     our newly opened DAS file. Link the information
!     for this file at the head of the file table list.
!
!     Set the output argument HANDLE as well.
!
      call lnkan  ( pool, new          )
      call lnkilb ( new,  fthead, pool )

      fthead           =   new
      fthan (fthead)   =   handle
      ftacc (fthead)   =   write
      ftlnk (fthead)   =   1

!
!     Fill in the file summary. We already know how many reserved
!     records and comment records there are. To find the number of the
!     first free record, the last logical address of each type, and the
!     locations of the last descriptors of each type, we must examine
!     the directory records. We do not assume that the data records in
!     the DAS file have been segregated.
!
      call cleari ( sumsiz, ftsum (1,fthead) )

      ftsum ( rrcidx, fthead )  =  locrrc
      ftsum ( rchidx, fthead )  =  locrch
      ftsum ( crcidx, fthead )  =  loccrc
      ftsum ( cchidx, fthead )  =  loccch

!
!     We'll need the logical unit connected to the file, so
!     we can read the file's directory records.
!
      call zzddhhlu ( handle, 'DAS', .false., number )

      if ( failed() ) then
         call chkout ( 'DASOPW' )
         return
      end if

!
!     We'll find the values for each data type separately.
!
      do type = 1, 3
!
!        The first directory record is located right after the
!        last comment record.
!
         nrec  =  locrrc + loccrc + 2

!
!        Keep track of the record number of the last data
!        record of the current type.
!
         ldrec(type)  =  0

!
!        Find the last directory containing a descriptor of a
!        record cluster of the current type.
!
         call zzdasgri( handle, nrec, dirrec )

         maxadr  =  dirrec ( rngbas  +  2 * type )
         nxtdir  =  dirrec ( fwdloc              )


         do while ( nxtdir .gt. 0 )
!
!           Read the directory record. If this record contains
!           descriptors for clusters we're interested in, update the
!           directory record number.
!
            call zzdasgri ( handle, nxtdir, dirrec )

            if ( failed() ) then
               call chkout ( 'DASOPW' )
               return
            end if


            if (  dirrec(rngbas + 2*type)  .gt.  0  ) then

               maxadr  =  dirrec ( rngbas  +  2 * type  )
               nrec    =  nxtdir

            end if

            nxtdir = dirrec ( fwdloc )

         end do

!
!        At this point, NREC is the record number of the directory
!        containing the last descriptor for clusters of TYPE, if
!        there are any such descriptors.
!
!        MAXADR is the maximum logical address of TYPE.
!
         ftsum ( llabas + type,  fthead )  =  maxadr

         if ( maxadr .gt. 0 ) then
            ftsum ( lrcbas + type,  fthead )  =  nrec
         else
            ftsum ( lrcbas + type,  fthead )  =  0
         end if
!
!        We still need to set the word location of the final
!        descriptor of TYPE, if there are any descriptors of TYPE.
!
         if ( maxadr .gt. 0 ) then
!
!           Re-read the directory record containing the last descriptor
!           of the current type.
!
            call zzdasgri ( handle, nrec, dirrec )

            if ( failed() ) then
               call chkout ( 'DASOPW' )
               return
            end if

!
!           Traverse the directory record, looking for the last
!           descriptor of TYPE. We'll keep track of the maximum logical
!           address of TYPE for each cluster of TYPE whose descriptor
!           we examine. When this value is the maximum logical address
!           of TYPE, we've found the last descriptor of TYPE.
!
!           Also keep track of the end record numbers for each
!           cluster.
!
            last    =  dirrec( rngbas  +  (2*type - 1)  )  -  1
            dsctyp  =  dirrec( begdsc                   )
            prvtyp  =  prev  ( dsctyp                   )
            endrec  =  nrec
            pos     =  begdsc

            do while ( last .lt. maxadr )

               pos = pos + 1

               if ( dirrec(pos) .gt. 0 ) then
                  curtyp = next(prvtyp)
               else
                  curtyp = prev(prvtyp)
               end if

               if ( curtyp .eq. type ) then
                  last = last  +  nw(type) * abs( dirrec(pos) )
               end if

               endrec = endrec + abs( dirrec(pos) )
               prvtyp = curtyp

            end do
!
!           At this point, POS is the word position of the last
!           descriptor of TYPE, and ENDREC is the record number
!           of the last data record of TYPE.
!
            ftsum ( lwdbas + type, fthead )  =  pos
            ldrec ( type )                   =  endrec


         else
!
!           There's no data of TYPE in the file.
!
            ftsum ( lwdbas + type, fthead )  =  0
            ldrec ( type )                   =  0

         end if

      end do

!
!     We're almost done; we need to find the number of the first free
!     record. This record follows all of the data records and all of
!     the directory records. It may happen that the last record in use
!     is an empty directory.
!
      call maxai ( ldrec, 3, ldrmax, loc )

      nrec   = locrrc + loccrc + 2

      call dasioi ( 'READ',  number,  nrec,  dirrec  )

      nxtrec = dirrec(fwdloc)


      do while ( nxtrec .ne. 0 )

         nrec   = nxtrec

         call dasioi ( 'READ',  number,  nrec,  dirrec  )

         nxtrec = dirrec(fwdloc)

      end do

!
!     Now NREC is the last directory record.
!
      ftsum ( freidx, fthead)  =  max ( ldrmax, nrec )  +  1

!
!     Insert the new handle into our handle set.
!
      call insrti ( handle, fhlist )


      call chkout ( 'DASOPW' )
      return





!$Procedure DASONW ( DAS, open new file )

      entry dasonw ( fname, ftype, ifname, ncomr, handle )

! $ Abstract
!
!     Open a new DAS file and set the file type.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     DAS
!     FILES
!
! $ Declarations
!
!     CHARACTER*(*)         FNAME
!     CHARACTER*(*)         FTYPE
!     CHARACTER*(*)         IFNAME
!     INTEGER               NCOMR
!     INTEGER               HANDLE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FNAME      I   Name of a DAS file to be opened.
!     FTYPE      I   Mnemonic code for type of data in the DAS file.
!     IFNAME     I   Internal file name.
!     NCOMR      I   Number of comment records to allocate.
!     HANDLE     O   Handle assigned to the opened DAS file.
!
! $ Detailed_Input
!
!     FNAME    is the name of a new DAS file to be created (and
!              consequently opened for write access).
!
!     FTYPE    is a string indicating the type of data placed into a DAS
!              file. The first nonblank character and the three, or
!              fewer, characters immediately following it are stored as
!              the part of the file's ID word following the forward
!              slash. It is an error if FTYPE is blank.
!
!              The file type may not contain any nonprinting characters.
!              FTYPE is case sensitive.
!
!              NAIF has reserved for its own use file types
!              consisting of the upper case letters (A-Z) and the
!              digits 0-9. NAIF recommends lower case or mixed case
!              file types be used by all others in order to avoid any
!              conflicts with NAIF file types.
!
!     IFNAME   is a string containing the internal file name for the new
!              file. The name may contain as many as 60 characters. This
!              should uniquely identify the file.
!
!     NCOMR    is the number of comment records to allocate.
!              Allocating comment records at file creation time may
!              reduce the likelihood of having to expand the
!              comment area later.
!
! $ Detailed_Output
!
!     HANDLE   is the file handle associated with the file. This
!              handle is used to identify the file in subsequent
!              calls to other DAS routines.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input filename is blank, the error
!         SPICE(BLANKFILENAME) is signaled.
!
!     2)  If the specified file cannot be opened without exceeding
!         the maximum allowed number of open DAS files, the error
!         SPICE(DASFTFULL) is signaled. No file will be created.
!
!     3)  If the file cannot be opened properly, an error is signaled
!         by a routine in the call tree of this routine. No file will
!         be created.
!
!     4)  If the initial records in the file cannot be written, an
!         error is signaled by a routine in the call tree of this
!         routine. No file will be created.
!
!     5)  If the file type is blank, the error SPICE(BLANKFILETYPE) is
!         signaled.
!
!     6)  If the file type contains nonprinting characters---decimal
!         0-31 and 127-255---, the error SPICE(ILLEGALCHARACTER) is
!         signaled.
!
!     7)  If the number of comment records allocated NCOMR is negative,
!         the error SPICE(INVALIDCOUNT) is signaled.
!
! $ Files
!
!     See argument FNAME.
!
! $ Particulars
!
!     The DAS files created by this routine have initialized file
!     records.
!
!     This entry point creates a new DAS file and sets the type of the
!     file to the mnemonic code passed to it.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Create a new DAS file and add 200 integers to it. Close the
!        file, then re-open it and read the data back out.
!
!
!        Example code begins here.
!
!
!              PROGRAM DASONW_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              CHARACTER*(*)         FNAME
!              PARAMETER           ( FNAME = 'dasonw_ex1.das' )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(4)         TYPE
!
!              INTEGER               DATA   ( 200 )
!
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               J
!
!        C
!        C     Open a new DAS file. Use the file name as the internal
!        C     file name, and reserve no records for comments.
!        C
!              TYPE = 'TEST'
!              CALL DASONW ( FNAME, TYPE, FNAME, 0, HANDLE )
!
!        C
!        C     Fill the array DATA with the integers 1 through
!        C     100, and add this array to the file.
!        C
!              DO I = 1, 100
!                 DATA(I) = I
!              END DO
!
!              CALL DASADI ( HANDLE, 100, DATA )
!
!        C
!        C     Now append the array DATA to the file again.
!        C
!              CALL DASADI ( HANDLE, 100, DATA )
!
!        C
!        C     Close the file.
!        C
!              CALL DASCLS ( HANDLE )
!
!        C
!        C     Now verify the addition of data by opening the
!        C     file for read access and retrieving the data.
!        C
!              CALL DASOPR ( FNAME, HANDLE )
!              CALL DASRDI ( HANDLE, 1, 200, DATA )
!
!        C
!        C     Dump the data to the screen.  We should see the
!        C     sequence  1, 2, ..., 100, 1, 2, ... , 100.
!        C
!              WRITE (*,*) ' '
!              WRITE (*,*) 'Data from "', FNAME, '":'
!              WRITE (*,*) ' '
!              DO I = 1, 20
!                 WRITE (*,'(10I5)') (DATA((I-1)*10+J), J = 1, 10)
!              END DO
!
!        C
!        C     Close the file.
!        C
!              CALL DASCLS ( HANDLE )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Data from "dasonw_ex1.das":
!
!            1    2    3    4    5    6    7    8    9   10
!           11   12   13   14   15   16   17   18   19   20
!           21   22   23   24   25   26   27   28   29   30
!           31   32   33   34   35   36   37   38   39   40
!           41   42   43   44   45   46   47   48   49   50
!           51   52   53   54   55   56   57   58   59   60
!           61   62   63   64   65   66   67   68   69   70
!           71   72   73   74   75   76   77   78   79   80
!           81   82   83   84   85   86   87   88   89   90
!           91   92   93   94   95   96   97   98   99  100
!            1    2    3    4    5    6    7    8    9   10
!           11   12   13   14   15   16   17   18   19   20
!           21   22   23   24   25   26   27   28   29   30
!           31   32   33   34   35   36   37   38   39   40
!           41   42   43   44   45   46   47   48   49   50
!           51   52   53   54   55   56   57   58   59   60
!           61   62   63   64   65   66   67   68   69   70
!           71   72   73   74   75   76   77   78   79   80
!           81   82   83   84   85   86   87   88   89   90
!           91   92   93   94   95   96   97   98   99  100
!
!
!        Note that after run completion, a new DAS file exists in the
!        output directory.
!
! $ Restrictions
!
!     None.
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     F.S. Turner        (JPL)
!
! $ Version
!
!-    SPICELIB Version 7.0.1, 19-JUL-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example.
!
!        Updated $Exceptions entry #3: error handling for the DAS open
!        failure is now performed by lower level code (ZZDDHOPN).
!
!-    SPICELIB Version 7.0.0, 26-FEB-2015 (NJB)
!
!        Now uses DAF/DAS handle manager subsystem.
!
!-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 6.0.0, 11-DEC-2001 (FST)
!
!        The DAS file ID word and internal file name are no longer
!        buffered by this routine. See DASFM's $Revisions section
!        for details.
!
!        The entry point was modified to insert the FTP validation
!        string, as well as the binary file format into the file record.
!
!-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 2.0.0, 31-AUG-1995 (NJB)
!
!        Changed argument list of the entry point DASONW. The input
!        argument NCOMR, which indicates the number of comment records
!        to reserve, was added to the argument list.
!
!-    SPICELIB Version 1.0.0, 29-OCT-1993 (KRG)
!
!-&


! $ Index_Entries
!
!     open a new DAS file
!     open a new DAS file with write access
!
!-&


! $ Revisions
!
!-    SPICELIB Version 6.0.0, 11-DEC-2001 (NJB) (FST)
!
!        See the $Revisions section under DASFM for a discussion of
!        the various changes made for this version.
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'DASONW' )

!
!     Initialize the file table pool and handle list, if necessary.
!
      if ( pass1 ) then

         call lnkini ( ftsize, pool   )
         call ssizei ( ftsize, fhlist )

         pass1 = .false.

      end if
!
!     Check to see whether the filename is blank. If it is, signal an
!     error, check out, and return.
!
      if ( fname .eq. ' ' ) then

         call setmsg ( 'The file name is blank. ' )
         call sigerr ( 'SPICE(BLANKFILENAME)'     )
         call chkout ( 'DASONW'                   )
         return

      end if
!
!     Check if the file type is blank.
!
      if ( ftype .eq. ' ' ) then

         call setmsg ( 'The file type is blank. ' )
         call sigerr ( 'SPICE(BLANKFILETYPE)'     )
         call chkout ( 'DASONW'                   )
         return

      end if
!
!     Check for nonprinting characters in the file type.
!
      fnb =  ltrim ( ftype )

      do i = fnb, rtrim ( ftype )

         if ( ( ichar ( ftype(i:i) ) .gt. maxpc )   .or. &
              ( ichar ( ftype(i:i) ) .lt. minpc ) ) then

            call setmsg ( 'The file type contains nonprinting' // &
                          ' characters. '                       )
            call sigerr ( 'SPICE(ILLEGALCHARACTER)'             )
            call chkout ( 'DASONW'                              )
            return

         end if

      end do

!
!     Validate the comment record count.
!
      if ( ncomr .lt. 0 ) then

         call setmsg ( 'The number of comment records allocated ' // &
                       'must be non-negative but was #.'           )
         call errint ( '#',  ncomr                                 )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                       )
         call chkout ( 'DASONW'                                    )
         return

      end if

!
!     Set the value the file type in a temporary variable to be sure of
!     its length and then set the value of the ID word. Only 4
!     characters are allowed for the file type, and they are the first
!     nonblank character and its three (3) immediate successors in the
!     input string FTYPE.
!
      ttype  = ftype(fnb:)
      idword = 'DAS/' // ttype

!
!     The file can be opened only if there is room for another file.
!
      if ( lnknfn(pool) .eq. 0  ) then

         call setmsg ( 'The file table is full, with # entries. '     // &
                       'Could not open ''#''.'                         )
         call errint ( '#', ftsize                                     )
         call errch  ( '#', fname                                      )
         call sigerr ( 'SPICE(DASFTFULL)'                              )
         call chkout ( 'DASONW'                                        )
         return

      end if

!
!     Open a new file.
!
      call zzddhopn ( fname, 'NEW', 'DAS', handle )

!
!     Get a logical unit for the file.
!
      call zzddhhlu ( handle, 'DAS', .false., number )

!
!     Fetch the system file format.
!
      call zzplatfm ( 'FILE_FORMAT', format )

!
!     Prepare to write the file record.
!
!     Use a local variable for the internal file name to ensure
!     that IFNLEN characters are written. The remaining
!     elements of the file record are:
!
!        -- the number of reserved records
!
!        -- the number of characters in use in the reserved
!           record area
!
!        -- the number of comment records
!
!        -- the number of characters in use in the comment area
!
!     Initially, all of these counts are zero, except for the
!     comment record count, which is set by the caller.
!
      locifn = ifname

      call zzdasnfr ( number, &
                      idword, &
                      locifn, &
                      0, &
                      0, &
                      ncomr, &
                      0, &
                      format  )

!
!     Check to see whether or not ZZDASNFR generated an error
!     writing the file record to the logical unit. In the event
!     an error occurs, checkout and return.
!
      if ( failed() ) then
         call chkout ( 'DASONW' )
         return
      end if

!
!     Zero out the first directory record in the file. If this
!     write fails, close the file with delete status and return
!     immediately. The first directory record follows the
!     comment records and reserved records. Currently there
!     are no reserved records, so the directory occupies record
!     NCOMR+2.
!
      call cleari ( nwi, dirrec )

      write ( unit   = number, &
              rec    = ncomr + 2, &
              iostat = iostat     ) dirrec

      if ( iostat .ne. 0 ) then
!
!        We had a write error. Ask the handle manager to
!        close and delete the new file.
!
         call zzddhcls ( handle, 'DAS', .true. )

         call chkout ( 'DASONW' )
         return

      end if

!
!     Update the file table to include information about
!     our newly opened DAS file. Link the information
!     for this file at the head of the file table list.
!
!     Set the output argument HANDLE as well.
!
      call lnkan  ( pool,   new             )
      call lnkilb ( new,    fthead,   pool  )

!
!     Clear out the file summary, except for the number of comment
!     records and the free record pointer. The free record pointer
!     should point to the first record AFTER the first directory.
!
      fthead                     =   new

      call cleari ( sumsiz, ftsum(1,fthead) )

      fthan (         fthead )   =   handle
      ftacc (         fthead )   =   write
      ftlnk (         fthead )   =   1
      ftsum ( freidx, fthead )   =   ncomr + 3
      ftsum ( crcidx, fthead )   =   ncomr

!
!     Insert the new handle into our handle set.
!
      call insrti ( handle, fhlist )


      call chkout ( 'DASONW' )
      return




!$Procedure DASOPN ( DAS, open new )

      entry dasopn ( fname, ifname, handle )

! $ Abstract
!
!     Obsolete: This routine has been superseded by DASONW, and it is
!     supported for purposes of backward compatibility only.
!
!     Open a new DAS file for writing.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     DAS
!     FILES
!
! $ Declarations
!
!     CHARACTER*(*)         FNAME
!     CHARACTER*(*)         IFNAME
!     INTEGER               HANDLE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FNAME      I   Name of a DAS file to be opened.
!     IFNAME     I   Internal file name.
!     HANDLE     O   Handle assigned to the opened DAS file.
!
! $ Detailed_Input
!
!     FNAME    is the name of a new DAS file to be created (and
!              consequently opened for write access).
!
!     IFNAME   is the internal file name for the new file. The name
!              may contain as many as 60 characters. This should
!              uniquely identify the file.
!
! $ Detailed_Output
!
!     HANDLE   is the file handle associated with the file. This
!              handle is used to identify the file in subsequent
!              calls to other DAS routines.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input filename is blank, the error
!         SPICE(BLANKFILENAME) is signaled.
!
!     2)  If the specified file cannot be opened without exceeding
!         the maximum allowed number of open DAS files, the error
!         SPICE(DASFTFULL) is signaled. No file will be created.
!
!     3)  If the file cannot be opened properly, an error is signaled
!         by a routine in the call tree of this routine. No file will
!         be created.
!
!     4)  If the initial records in the file cannot be written, an
!         error is signaled by a routine in the call tree of this
!         routine. No file will be created.
!
!     5)  If no logical units are available, an error is signaled by a
!         routine in the call tree of this routine. No file will be
!         created.
!
! $ Files
!
!     See argument FNAME.
!
! $ Particulars
!
!     The DAS files created by this routine have initialized file
!     records.
!
!     This entry point has been made obsolete by the entry point DASONW,
!     and it is supported for reasons of backward compatibility only.
!     New software development should use the entry point DASONW.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Create a new DAS file and add 200 integers to it. Close the
!        file, then re-open it and read the data back out.
!
!
!        Example code begins here.
!
!
!              PROGRAM DASOPN_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              CHARACTER*(*)         FNAME
!              PARAMETER           ( FNAME = 'dasopn_ex1.das' )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(4)         TYPE
!
!              INTEGER               DATA   ( 200 )
!
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               J
!
!        C
!        C     Open a new DAS file. Use the file name as the internal
!        C     file name.
!        C
!              TYPE = 'TEST'
!              CALL DASOPN ( FNAME, FNAME, HANDLE )
!
!        C
!        C     Fill the array DATA with the integers 1 through
!        C     100, and add this array to the file.
!        C
!              DO I = 1, 100
!                 DATA(I) = I
!              END DO
!
!              CALL DASADI ( HANDLE, 100, DATA )
!
!        C
!        C     Now append the array DATA to the file again.
!        C
!              CALL DASADI ( HANDLE, 100, DATA )
!
!        C
!        C     Close the file.
!        C
!              CALL DASCLS ( HANDLE )
!
!        C
!        C     Now verify the addition of data by opening the
!        C     file for read access and retrieving the data.
!        C
!              CALL DASOPR ( FNAME, HANDLE )
!              CALL DASRDI ( HANDLE, 1, 200, DATA )
!
!        C
!        C     Dump the data to the screen.  We should see the
!        C     sequence  1, 2, ..., 100, 1, 2, ... , 100.
!        C
!              WRITE (*,*) ' '
!              WRITE (*,*) 'Data from "', FNAME, '":'
!              WRITE (*,*) ' '
!              DO I = 1, 20
!                 WRITE (*,'(10I5)') (DATA((I-1)*10+J), J = 1, 10)
!              END DO
!
!        C
!        C     Close the file.
!        C
!              CALL DASCLS ( HANDLE )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Data from "dasopn_ex1.das":
!
!            1    2    3    4    5    6    7    8    9   10
!           11   12   13   14   15   16   17   18   19   20
!           21   22   23   24   25   26   27   28   29   30
!           31   32   33   34   35   36   37   38   39   40
!           41   42   43   44   45   46   47   48   49   50
!           51   52   53   54   55   56   57   58   59   60
!           61   62   63   64   65   66   67   68   69   70
!           71   72   73   74   75   76   77   78   79   80
!           81   82   83   84   85   86   87   88   89   90
!           91   92   93   94   95   96   97   98   99  100
!            1    2    3    4    5    6    7    8    9   10
!           11   12   13   14   15   16   17   18   19   20
!           21   22   23   24   25   26   27   28   29   30
!           31   32   33   34   35   36   37   38   39   40
!           41   42   43   44   45   46   47   48   49   50
!           51   52   53   54   55   56   57   58   59   60
!           61   62   63   64   65   66   67   68   69   70
!           71   72   73   74   75   76   77   78   79   80
!           81   82   83   84   85   86   87   88   89   90
!           91   92   93   94   95   96   97   98   99  100
!
!
!        Note that after run completion, a new DAS file exists in the
!        output directory.
!
! $ Restrictions
!
!     None.
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     F.S. Turner        (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 7.0.1, 19-JUL-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example.
!
!        Updated $Exceptions entry #3: error handling for the DAS open
!        failure is now performed by lower level code (ZZDDHOPN).
!
!-    SPICELIB Version 7.0.0, 30-JUL-2014 (NJB)
!
!        Now uses DAF/DAS handle manager subsystem.
!
!-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 6.0.0, 11-DEC-2001 (FST)
!
!        The DAS file ID word and internal file name are no longer
!        buffered by this routine. See DASFM's $Revisions section
!        for details.
!
!        This entry point was modified to insert the FTP validation
!        string, as well as the binary file format into the file record.
!
!-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 2.0.0, 29-OCT-1993 (KRG)
!
!        The effect of this routine is unchanged. It still uses the ID
!        word 'NAIF/DAS'. This is for backward compatibility only.
!
!        Added statements to the $Abstract and $Particulars sections
!        to document that this entry is now considered to be obsolete,
!        and that it has been superseded by the entry point DASONW.
!
!        Added a test for a blank filename before attempting to use the
!        filename in the routine. If the filename is blank, the error
!        SPICE(BLANKFILENAME) will be signaled.
!
!-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     open a new DAS file for writing
!     open a new DAS file for write access
!
!-&


! $ Revisions
!
!-    SPICELIB Version 7.0.0, 30-JUL-2014 (NJB)
!
!        Now uses DAF/DAS handle manager subsystem.
!
!-    SPICELIB Version 6.0.0, 11-DEC-2001 (FST)
!
!        See the $Revisions section under DASFM for a discussion
!        of the changes made for this version.
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'DASOPN' )

!
!     Initialize the file table pool and handle list, if necessary.
!
      if ( pass1 ) then

         call lnkini ( ftsize, pool   )
         call ssizei ( ftsize, fhlist )

         pass1 = .false.

      end if
!
!     Check to see whether the filename is blank. If it is, signal an
!     error, check out, and return.
!
      if ( fname .eq. ' ' ) then

         call setmsg ( 'The file name is blank. ' )
         call sigerr ( 'SPICE(BLANKFILENAME)'     )
         call chkout ( 'DASOPN'                   )
         return

      end if

!
!     The file can be opened only if there is room for another file.
!
      if ( lnknfn(pool) .eq. 0  ) then

         call setmsg ( 'The file table is full, with # entries. '     // &
                       'Could not open ''#''.'                         )
         call errint ( '#', ftsize                                     )
         call errch  ( '#', fname                                      )
         call sigerr ( 'SPICE(DASFTFULL)'                              )
         call chkout ( 'DASOPN'                                        )
         return


      end if

!
!     To open a new file: get a free unit, open the file, write
!     the file record, and set the number of links to one.
!
!     Look out for:
!
!        -- No free logical units.
!
!        -- Error opening the file.
!
!        -- Error writing to the file.
!
!     If anything goes wrong after the file has been opened, delete
!     the file.
!
!
      call zzddhopn ( fname, 'NEW', 'DAS', handle )

      call zzddhhlu ( handle, 'DAS', .false., number )

      if ( failed() ) then
         call chkout ( 'DASOPN' )
         return
      end if

!
!     Fetch the system file format.
!
      call zzplatfm ( 'FILE_FORMAT', format )

!
!     Prepare to write the file record.
!
!     Use a local variable for the internal file name to ensure that
!     IFNLEN characters are written. The remaining elements of the file
!     record are:
!
!        -- the number of reserved records
!
!        -- the number of characters in use in the reserved
!           record area
!
!        -- the number of comment records
!
!        -- the number of characters in use in the comment area
!
!     Initially, all of these counts are zero.
!
!
      locifn = ifname
      idword = 'NAIF/DAS'

      call zzdasnfr ( number, &
                      idword, &
                      locifn, &
                      0, &
                      0, &
                      0, &
                      0, &
                      format  )

      if ( failed() ) then
         call chkout ( 'DASOPN' )
         return
      end if

!
!     Zero out the first directory record (record #2) in the file. If
!     this write fails, close the file with delete status and return
!     immediately.
!

!
!     NOTE: re-write this using ZZDDHCLS.
!
      call cleari (  nwi,               dirrec )
      call dasioi ( 'WRITE', number, 2, dirrec )

      if ( failed() ) then
         close       ( unit = number,  status = 'DELETE' )
         call chkout ( 'DASOPN' )
         return
      end if

!
!     Update the file table to include information about
!     our newly opened DAS file. Link the information
!     for this file at the head of the file table list.
!
!     Set the output argument HANDLE as well.
!
      call lnkan  ( pool,   new             )
      call lnkilb ( new,    fthead,   pool  )

!
!     Clear out the file summary, except for the free record pointer.
!     The free record pointer should point to the first record AFTER
!     the first directory.
!
      fthead                     =   new

      call cleari ( sumsiz, ftsum(1,fthead) )

      fthan (         fthead )   =   handle
      ftacc (         fthead )   =   write
      ftlnk (         fthead )   =   1
      ftsum ( freidx, fthead )   =   3

!
!     Insert the new handle into our handle set.
!
      call insrti ( handle, fhlist )

      call chkout ( 'DASOPN' )
      return




!$Procedure DASOPS ( DAS, open scratch )

      entry dasops ( handle )

! $ Abstract
!
!     Open a scratch DAS file for writing.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     DAS
!     FILES
!     UTILITY
!
! $ Declarations
!
!     INTEGER               HANDLE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     O   Handle assigned to a scratch DAS file.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     HANDLE   is the file handle associated with the scratch file
!              opened by this routine. This handle is used to
!              identify the file in subsequent calls to other DAS
!              routines.
!
! $ Parameters
!
!     FTSIZE   is the maximum number of DAS files that a user can have
!              open simultaneously. This includes any files used by the
!              DAS system when closing files opened with write access.
!              Currently, DASCLS (via the SPICELIB routine DASSDR) opens
!              a scratch DAS file using DASOPS to segregate (sort by
!              data type) the records in the DAS file being closed.
!              Segregating the data by type improves the speed of access
!              to the data.
!
!              In order to avoid the possibility of overflowing the
!              DAS file table we recommend, when at least one DAS
!              file is open with write access, that users of this
!              software limit themselves to at most FTSIZE - 2 other
!              open DAS files. If no files are to be open with write
!              access, then users may open FTSIZE files with no
!              possibility of overflowing the DAS file table.
!
! $ Exceptions
!
!     1)  If the specified file cannot be opened without exceeding
!         the maximum allowed number of open DAS files, the error
!         SPICE(DASFTFULL) is signaled. No file will be created.
!
!     2)  If file cannot be opened properly, an error is signaled by a
!         routine in the call tree of this routine. No file will be
!         created.
!
!     3)  If the initial records in the file cannot be written, the
!         error SPICE(DASWRITEFAIL) is signaled. No file will be
!         created.
!
!     4)  If no logical units are available, an error is signaled by a
!         routine in the call tree of this routine. No file will be
!         created.
!
! $ Files
!
!     See output argument HANDLE.
!
!     See FTSIZE in the $Parameters section for a description of a
!     potential problem with overflowing the DAS file table when at
!     least one DAS file is opened with write access.
!
! $ Particulars
!
!     This routine is a utility used by the DAS system to provide
!     work space needed when creating new DAS files.
!
!     The DAS files created by this routine have initialized file
!     records. The file type for a DAS scratch file is 'SCR ', so the
!     file type 'SCR ' is not available for general use. As with new
!     permanent files, these files are opened for write access. DAS
!     files opened by DASOPS are automatically deleted when they are
!     closed.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Create a DAS scratch file containing 10 integers, 5 double
!        precision numbers, and 4 characters, then print the logical
!        address ranges in use.
!
!
!        Example code begins here.
!
!
!              PROGRAM DASOPS_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local variables.
!        C
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               LASTC
!              INTEGER               LASTD
!              INTEGER               LASTI
!
!        C
!        C     Use a scratch file, since there's no reason to keep
!        C     the file.
!        C
!              CALL DASOPS ( HANDLE )
!
!              DO I = 1, 10
!                 CALL DASADI ( HANDLE, 1, I )
!              END DO
!
!              DO I = 1, 5
!                 CALL DASADD ( HANDLE, 1, DBLE(I) )
!              END DO
!
!        C
!        C     Add character data to the file. DAS character data are
!        C     treated as a character array, not as a string. The
!        C     following call adds only the first 4 characters to the
!        C     DAS file.
!        C
!              CALL DASADC ( HANDLE, 4, 1, 4, 'SPUDWXY' )
!
!        C
!        C     Now check the logical address ranges.
!        C
!              CALL DASLLA ( HANDLE, LASTC, LASTD, LASTI )
!
!              WRITE (*,*) 'Last character address in use: ', LASTC
!              WRITE (*,*) 'Last d.p. address in use     : ', LASTD
!              WRITE (*,*) 'Last integer address in use  : ', LASTI
!
!        C
!        C     Scratch files are automatically deleted when they are
!        C     closed.
!        C
!              CALL DASCLS ( HANDLE )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Last character address in use:            4
!         Last d.p. address in use     :            5
!         Last integer address in use  :           10
!
!
! $ Restrictions
!
!     None.
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 7.0.1, 19-JUL-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example.
!
!        Extended $Particulars section to indicate that scratch files
!        are deleted when they are closed.
!
!        Updated $Exceptions entry #2: error handling for the DAS open
!        failure is now performed by lower level code (ZZDDHOPN), and
!        added FTSIZE parameter description.
!
!-    SPICELIB Version 7.0.0, 07-APR-2016 (NJB)
!
!        Now uses DAF/DAS handle manager subsystem.
!
!-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 2.0.0, 29-OCT-1993 (KRG)
!
!        Modified the entry point to use the new file ID format which
!        contains a mnemonic code for the data type.
!
!        Put meaningful values into the type and internal filename
!        for a DAS scratch file, rather than leaving them blank.
!
!        Documented the potential problem of overflowing the DAS file
!        table when attempting to close a DAS file opened with write
!        access when the file table is full. Modified the long error
!        message to indicate this as a cause of the problem.
!
!-    SPICELIB Version 1.1.0, 04-MAY-1993 (NJB)
!
!        Bug fix: removed file name variable from error message.
!
!-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     open a scratch DAS file
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.0.0, 29-OCT-1993 (KRG)
!
!        Modified the entry point to use the new file ID format which
!        contains a mnemonic code for the data type.
!
!        DAS scratch files use the type 'SCR ', so the ID word for a DAS
!        scratch file would be: 'DAS/SCR '
!
!        Changed the internal filename from blank to the string:
!
!           'DAS SCRATCH FILE'
!
!        It's probably better to have something written there than
!        nothing.
!
!        Documented the potential problem of overflowing the DAS file
!        table when attempting to close a DAS file opened with write
!        access when the file table is full. Modified the long error
!        message to indicate this as a cause of the problem.
!
!        The problem occurs when the file table is full, the number of
!        open DAS files equals FTSIZE, and at least one of the open
!        files was opened with write access. If an attempt to close a
!        file opened with write access is made under these conditions,
!        by calling DASCLS, it will fail. DASCLS (via DASSDR) calls
!        DASOPS to open a scratch DAS file, but the scratch file CANNOT
!        be opened because the file table is full. If this occurs, close
!        a file open for read access, or restrict the number of open
!        files in use to be at most FTSIZE - 1 when there will be at
!        least one file opened with write access.
!
!-    SPICELIB Version 1.1.0, 04-MAY-1993 (NJB)
!
!        Bug fix: removed unneeded file name variable FNAME from
!        error message.
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'DASOPS' )

!
!     Initialize the file table pool and handle list, if necessary.
!
      if ( pass1 ) then

         call lnkini ( ftsize, pool   )
         call ssizei ( ftsize, fhlist )

         pass1 = .false.

      end if

!
!     The file can be opened only if there is room for another file.
!
      if ( lnknfn(pool) .eq. 0  ) then

         call setmsg ( 'The file table is full, with # entries.'      // &
                       ' Could not open a scratch file. If a call'    // &
                       ' to DASOPS was not made and this error'       // &
                       ' occurred, it is likely that the DAS file'    // &
                       ' table was full and an attempt to close a'    // &
                       ' file opened with write access was made. See' // &
                       ' the DAS required reading and DASFM for'      // &
                       ' details.'                                     )
         call errint ( '#', ftsize                                     )
         call sigerr ( 'SPICE(DASFTFULL)'                              )
         call chkout ( 'DASOPS'                                        )
         return

      end if

!
!     Assign a name to the scratch file. This name is required
!     by the DDH subsystem.
!
      locdas = 'DAS SCRATCH FILE'

!
!     Open a DAS file for scratch access.
!
      call zzddhopn ( locdas, 'SCRATCH', 'DAS', handle )

      if ( failed() ) then
         call chkout ( 'DASOPS' )
         return
      end if

!
!     Get a logical unit for the file.
!
      call zzddhhlu ( handle, 'DAS', .false., number )

!
!     Fetch the system file format.
!
      call zzplatfm ( 'FILE_FORMAT', format )

!
!     Prepare to write the file record.
!
!     Use a local variable for the internal file name to ensure
!     that IFNLEN characters are written. The remaining
!     elements of the file record are:
!
!        -- the number of reserved records
!
!        -- the number of characters in use in the reserved
!           record area
!
!        -- the number of comment records
!
!        -- the number of characters in use in the comment area
!
!     Initially, all of these counts are zero, except for the
!     comment record count, which is set by the caller.
!
      locifn = locdas(:ifnlen)

      call zzdasnfr ( number, &
                      idword, &
                      locifn, &
                      0, &
                      0, &
                      0, &
                      0, &
                      format  )

!
!     Check to see whether or not ZZDASNFR generated an error
!     writing the file record to the logical unit. In the event
!     an error occurs, checkout and return.
!
      if ( failed() ) then
         call chkout ( 'DASOPS' )
         return
      end if

!
!     Update the file table to include information about
!     our newly opened DAS file. Link the information
!     for this file at the head of the file table list.
!
!     Set the output argument HANDLE as well.
!
      call lnkan  ( pool,   new             )
      call lnkilb ( new,    fthead,   pool  )

      fthead                     =   new

!
!     Clear out the file summary, except for the free record pointer.
!     The free record pointer should point to the first record AFTER
!     the first directory.
!
      call cleari ( sumsiz, ftsum(1,fthead) )

      fthan (         fthead )   =   handle
      ftacc (         fthead )   =   write
      ftlnk (         fthead )   =   1
      ftsum ( freidx, fthead )   =   3

!
!     Insert the new handle into our handle set.
!
      call insrti ( handle, fhlist )


      call chkout ( 'DASOPS' )
      return



!$Procedure DASLLC ( DAS, low-level close )

      entry dasllc ( handle )

! $ Abstract
!
!     Close the DAS file associated with a given handle, without
!     flushing buffered data or segregating the file.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     DAS
!     FILES
!
! $ Declarations
!
!     INTEGER               HANDLE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of a DAS file to be closed.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a previously opened DAS file.
!
! $ Detailed_Output
!
!     None.
!
!     See $Particulars for a description of the effect of this routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the specified handle does not belong to a DAS file that is
!         currently open, this routine returns without signaling an
!         error.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     Normally, routines outside of SPICELIB will not need to call this
!     routine. Application programs should close DAS files by calling
!     the SPICELIB routine DASCLS. This routine is a lower-level
!     routine that is called by DASCLS, but (obviously) does not have
!     the full functionality of DASCLS.
!
!     This routine closes a DAS file and updates the DAS file manager's
!     bookkeeping information on open DAS files. Because the DAS file
!     manager must keep track of which files are open at any given time,
!     it is important that DAS files be closed only with DASCLS or
!     DASLLC, to prevent the remaining DAS routines from failing,
!     sometimes mysteriously.
!
!     Note that when a file is opened more than once for read or write
!     access, DASOPR returns the same handle each time it is re-opened.
!     Each time the file is closed, DASLLC checks to see if any other
!     claims on the file are still active before physically closing
!     the file.
!
!     Unlike DASCLS, this routine does not force a write of updated,
!     buffered records to the indicated file, nor does it segregate the
!     data records in the file.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Write a DAS file by adding data to it over multiple passes.
!        Avoid spending time on file segregation between writes.
!
!        Each pass opens the file, adds character, double precision,
!        and integer data to the file, writes out buffered data by
!        calling DASWBR, and closes the file without segregating the
!        data by calling DASLLC.
!
!        The program also checks the file: after the final write,
!        the program reads the data and compares it to expected values.
!
!        Note that most user-oriented applications should segregate a
!        DAS file after writing it, since this greatly enhances file
!        reading efficiency. The technique demonstrated here may be
!        useful for cases in which a file will be written via many
!        small data additions, and in which the file is read between
!        write operations.
!
!
!        Example code begins here.
!
!
!              PROGRAM DASLLC_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters
!        C
!              INTEGER               FILSIZ
!              PARAMETER           ( FILSIZ = 255 )
!
!              INTEGER               FTYPLN
!              PARAMETER           ( FTYPLN = 3 )
!
!              INTEGER               CHRLEN
!              PARAMETER           ( CHRLEN = 50 )
!
!              INTEGER               IBUFSZ
!              PARAMETER           ( IBUFSZ = 20 )
!
!              INTEGER               DBUFSZ
!              PARAMETER           ( DBUFSZ = 30 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(CHRLEN)    CHRBUF
!              CHARACTER*(FILSIZ)    FNAME
!              CHARACTER*(FTYPLN)    FTYPE
!              CHARACTER*(CHRLEN)    XCHRBF
!
!              DOUBLE PRECISION      DPBUF  ( DBUFSZ )
!              DOUBLE PRECISION      XDPBUF ( DBUFSZ )
!
!              INTEGER               FIRSTC
!              INTEGER               FIRSTD
!              INTEGER               FIRSTI
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               INTBUF ( IBUFSZ )
!              INTEGER               J
!              INTEGER               LASTC
!              INTEGER               LASTD
!              INTEGER               LASTI
!              INTEGER               NCALL
!              INTEGER               NCOMR
!              INTEGER               NPASS
!              INTEGER               PASSNO
!              INTEGER               XINTBF ( IBUFSZ )
!
!
!        C
!        C     Initial values
!        C
!              DATA                  FNAME  / 'dasllc_ex1.das' /
!              DATA                  FTYPE  / 'ANG' /
!              DATA                  NCALL  / 1000  /
!              DATA                  NCOMR  / 10    /
!              DATA                  NPASS  / 3     /
!
!        C
!        C     Open a new DAS file. We'll allocate NCOMR records
!        C     for comments. The file type is not one of the standard
!        C     types recognized by SPICE; however it can be used to
!        C     ensure the database file is of the correct type.
!        C
!        C     We'll use the file name as the internal file name.
!        C
!              CALL DASONW ( FNAME, FTYPE, FNAME, NCOMR, HANDLE )
!
!        C
!        C     Add data of character, integer, and double precision
!        C     types to the file in interleaved fashion. We'll add to
!        C     the file over NPASS "passes," in each of which we close
!        C     the file after writing.
!        C
!              DO PASSNO = 1, NPASS
!
!                 IF ( PASSNO .GT. 1 ) THEN
!
!                    WRITE (*,*) 'Opening file for write access...'
!
!                    CALL DASOPW( FNAME, HANDLE )
!
!                 END IF
!
!                 DO I = 1, NCALL
!        C
!        C           Add string data to the file.
!        C
!                    CHRBUF = 'Character value #'
!                    CALL REPMI( CHRBUF, '#', I, CHRBUF )
!
!                    CALL DASADC ( HANDLE, CHRLEN, 1, CHRLEN, CHRBUF )
!
!        C
!        C           Add double precision data to the file.
!        C
!                    DO J = 1, DBUFSZ
!                       DPBUF(J) = DBLE( 100000000*PASSNO + 100*I + J )
!                    END DO
!
!                    CALL DASADD ( HANDLE, DBUFSZ, DPBUF )
!
!        C
!        C           Add integer data to the file.
!        C
!                    DO J = 1, IBUFSZ
!                       INTBUF(J) = 100000000*PASSNO  +  100 * I  +  J
!                    END DO
!
!                    CALL DASADI ( HANDLE, IBUFSZ, INTBUF )
!
!                 END DO
!
!        C
!        C        Write buffered data to the file.
!        C
!                 WRITE (*,*) 'Writing buffered data...'
!                 CALL DASWBR ( HANDLE )
!
!        C
!        C        Close the file without segregating it.
!        C
!                 WRITE (*,*) 'Closing DAS file...'
!                 CALL DASLLC ( HANDLE )
!
!              END DO
!
!              WRITE (*,*) 'File write is done.'
!
!        C
!        C     Check file contents.
!        C
!              CALL DASOPR( FNAME, HANDLE )
!
!        C
!        C     Read data from the file; compare to expected values.
!        C
!        C     Initialize end addresses.
!        C
!              LASTC = 0
!              LASTD = 0
!              LASTI = 0
!
!              DO PASSNO = 1, NPASS
!
!                 DO I = 1, NCALL
!        C
!        C           Check string data.
!        C
!                    XCHRBF = 'Character value #'
!                    CALL REPMI( XCHRBF, '#', I, XCHRBF )
!
!                    FIRSTC = LASTC + 1
!                    LASTC  = LASTC + CHRLEN
!
!                    CALL DASRDC ( HANDLE, FIRSTC, LASTC,
!             .                    1,      CHRLEN, CHRBUF )
!
!                    IF ( CHRBUF .NE. XCHRBF ) THEN
!                       WRITE (*,*) 'Character data mismatch: '
!                       WRITE (*,*) 'PASS     = ', PASSNO
!                       WRITE (*,*) 'I        = ', I
!                       WRITE (*,*) 'Expected = ', XCHRBF
!                       WRITE (*,*) 'Actual   = ', CHRBUF
!                       STOP
!                    END IF
!
!        C
!        C           Check double precision data.
!        C
!                    DO J = 1, DBUFSZ
!                       XDPBUF(J) = DBLE(   100000000*PASSNO
!             .                           + 100*I + J        )
!                    END DO
!
!                    FIRSTD = LASTD + 1
!                    LASTD  = LASTD + DBUFSZ
!
!                    CALL DASRDD ( HANDLE, FIRSTD, LASTD, DPBUF )
!
!                    DO J = 1, DBUFSZ
!
!                       IF ( DPBUF(J) .NE. XDPBUF(J) ) THEN
!
!                          WRITE (*,*)
!             .                      'Double precision data mismatch: '
!                          WRITE (*,*) 'PASS     = ', PASSNO
!                          WRITE (*,*) 'I        = ', I
!                          WRITE (*,*) 'J        = ', J
!                          WRITE (*,*) 'Expected = ', XDPBUF(J)
!                          WRITE (*,*) 'Actual   = ', DPBUF(J)
!                          STOP
!
!                       END IF
!
!                    END DO
!
!        C
!        C           Check integer data.
!        C
!                    DO J = 1, IBUFSZ
!                       XINTBF(J) = 100000000*PASSNO  +  100 * I  +  J
!                    END DO
!
!                    FIRSTI = LASTI + 1
!                    LASTI  = LASTI + IBUFSZ
!
!                    CALL DASRDI ( HANDLE, FIRSTI, LASTI, INTBUF )
!
!                    DO J = 1, IBUFSZ
!
!                       IF ( INTBUF(J) .NE. XINTBF(J) ) THEN
!
!                          WRITE (*,*) 'Integer data mismatch: '
!                          WRITE (*,*) 'PASS     = ', PASSNO
!                          WRITE (*,*) 'I        = ', I
!                          WRITE (*,*) 'J        = ', J
!                          WRITE (*,*) 'Expected = ', XINTBF(J)
!                          WRITE (*,*) 'Actual   = ', INTBUF(J)
!                          STOP
!
!                       END IF
!
!                    END DO
!
!                 END DO
!
!              END DO
!
!              WRITE (*,*) 'File check is done.'
!
!        C
!        C     Close the file.
!        C
!              CALL DASCLS ( HANDLE )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Writing buffered data...
!         Closing DAS file...
!         Opening file for write access...
!         Writing buffered data...
!         Closing DAS file...
!         Opening file for write access...
!         Writing buffered data...
!         Closing DAS file...
!         File write is done.
!         File check is done.
!
!
!        Note that after run completion, a new DAS file exists in the
!        output directory.
!
! $ Restrictions
!
!     None.
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 7.0.1, 19-JUL-2021 (NJB) (JDR)
!
!        Updated the header to comply with NAIF standard. Added
!        complete code example.
!
!-    SPICELIB Version 7.0.0, 30-JUL-2014 (NJB)
!
!        Now uses DAF/DAS handle manager subsystem.
!
!-    SPICELIB Version 6.0.3, 10-APR-2014 (NJB)
!
!        Corrected header comments: routine that flushes
!        written, buffered records is DASWBR, not DASWUR.
!
!-    SPICELIB Version 6.0.2, 21-FEB-2003 (NJB)
!
!        Corrected inline comment: determination of whether file
!        is open is done by searching the handle column of the file
!        table, not the unit column.
!
!-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if these open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     close a DAS file
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if these open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'DASLLC' )

!
!     Initialize the file table pool and handle list, if necessary.
!
      if ( pass1 ) then

         call lnkini ( ftsize, pool   )
         call ssizei ( ftsize, fhlist )

         pass1 = .false.

      end if

!
!     Is this file even open?  Peruse the `handle' column of the file
!     table; see whether this handle is present.
!
      findex  =  fthead
      found   = .false.

      do while (  ( .not. found )  .and.  ( findex .gt. 0 )  )

         if ( fthan(findex) .eq. handle ) then
            found = .true.
         else
            findex = lnknxt ( findex, pool )
         end if

      end do

!
!     If the file is not open: no harm, no foul.  Otherwise, decrement
!     the number of links to the file.  If the number of links drops to
!     zero, physically close the file and remove it from the file
!     buffer.
!
      if ( found ) then

         ftlnk(findex) = ftlnk(findex) - 1

         if ( ftlnk(findex) .eq. 0 ) then
!
!           Close this file and delete it from the active list.
!           If this was the head node of the list, the head node
!           becomes the successor of this node (which may be NIL).
!           Delete the handle from our handle set.
!
            call zzddhcls ( handle, 'DAS', .false. )

            if ( failed() ) then
               call chkout ( 'DASLLC' )
               return
            end if

            if ( findex .eq. fthead ) then
               fthead = lnknxt ( findex, pool )
            end if

            call lnkfsl ( findex, findex, pool )

            call removi ( handle, fhlist       )

         end if

      end if

      call chkout ( 'DASLLC' )
      return


!$Procedure DASHFS ( DAS, handle to file summary )

      entry dashfs ( handle, &
                     nresvr, &
                     nresvc, &
                     ncomr, &
                     ncomc, &
                     free, &
                     lastla, &
                     lastrc, &
                     lastwd )

! $ Abstract
!
!     Return a file summary for a specified DAS file.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     DAS
!     FILES
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     INTEGER               HANDLE
!     INTEGER               NRESVR
!     INTEGER               NRESVC
!     INTEGER               NCOMR
!     INTEGER               NCOMC
!     INTEGER               FREE
!     INTEGER               LASTLA ( 3 )
!     INTEGER               LASTRC ( 3 )
!     INTEGER               LASTWD ( 3 )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of a DAS file.
!     NRESVR     O   Number of reserved records in file.
!     NRESVC     O   Number of characters in use in reserved rec. area.
!     NCOMR      O   Number of comment records in file.
!     NCOMC      O   Number of characters in use in comment area.
!     FREE       O   Number of first free record.
!     LASTLA     O   Array of last logical addresses for each data type.
!     LASTRC     O   Record number of last descriptor of each data type.
!     LASTWD     O   Word number of last descriptor of each data type.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a previously opened DAS file. The file
!              may be open for read or write access.
!
! $ Detailed_Output
!
!     NRESVR   is the number of reserved records in a specified DAS
!              file.
!
!     NRESVC   is the number of characters in use in the reserved record
!              area of a specified DAS file.
!
!     NCOMR    is the number of comment records in a specified DAS file.
!
!     NCOMC    is the number of characters in use in the comment area of
!              a specified DAS file.
!
!     FREE     is the 1-based record number of the first free record in
!              a specified DAS file.
!
!     LASTLA   is an array containing the highest current 1-based
!              logical addresses, in the specified DAS file, of data of
!              character, double precision, and integer types, in that
!              order.
!
!     LASTRC   is an array containing the 1-based record numbers, in the
!              specified DAS file, of the directory records containing
!              the current last descriptors of clusters of character,
!              double precision, and integer data records, in that
!              order.
!
!     LASTWD   is an array containing the 1-based word indices, within
!              the respective descriptor records identified by the
!              elements of LASTRC, of the current last descriptors of
!              clusters of character, double precision, and integer data
!              records, in that order.
!
! $ Parameters
!
!     See INCLUDE file das.inc for declarations and descriptions of
!     parameters used throughout the DAS system.
!
!     CHARDT,
!     DPDT,
!     INTDT    are data type specifiers that indicate CHARACTER,
!              DOUBLE PRECISION, and INTEGER respectively. These
!              parameters are used in all DAS routines that require a
!              data type specifier.
!
! $ Exceptions
!
!     1)  If the specified handle does not belong to any file that is
!         currently known to be open, the error SPICE(DASNOSUCHHANDLE)
!         is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The quantities
!
!        NRESVR
!        NRESVC
!        NCOMR
!        NCOMC
!        FREE
!        LASTLA
!        LASTRC
!        LASTWD
!
!     define the "state" of a DAS file, and in particular the state of
!     the directory structure of the file. This information is needed by
!     other DAS routines, but application programs will usually have no
!     need for it. The one exception is the array of "last" logical
!     addresses LASTLA: these addresses indicate how many words of data
!     of each type are contained in the specified DAS file. The elements
!     of LASTLA can be conveniently retrieved by calling DASLLA.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Create a DAS file containing 10 integers, 5 double precision
!        numbers, and 4 characters. Print the summary of the file and
!        dump its contents.
!
!
!        Example code begins here.
!
!
!              PROGRAM DASHFS_EX1
!              IMPLICIT NONE
!
!              INCLUDE 'das.inc'
!
!        C
!        C     Local parameters.
!        C
!              CHARACTER*(*)         FNAME
!              PARAMETER           ( FNAME = 'dashfs_ex1.das' )
!
!              INTEGER               IFNMLN
!              PARAMETER           ( IFNMLN = 60 )
!
!              INTEGER               LINLEN
!              PARAMETER           ( LINLEN = 2  )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(IFNMLN)    IFNAME
!              CHARACTER*(4)         TYPE
!              CHARACTER*(LINLEN)    LINE
!
!              DOUBLE PRECISION      X
!
!              INTEGER               FIRST
!              INTEGER               FREE
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               LAST
!              INTEGER               LASTLA ( 3 )
!              INTEGER               LASTRC ( 3 )
!              INTEGER               LASTWD ( 3 )
!              INTEGER               N
!              INTEGER               NCOMC
!              INTEGER               NCOMR
!              INTEGER               NREAD
!              INTEGER               NRESVC
!              INTEGER               NRESVR
!              INTEGER               REMAIN
!
!        C
!        C     Open a new DAS file. Reserve no records for comments.
!        C
!              TYPE   = 'TEST'
!              IFNAME = 'TEST.DAS/NAIF/NJB/11-NOV-1992-20:12:20'
!
!              CALL DASONW ( FNAME, TYPE, IFNAME, 0, HANDLE )
!
!        C
!        C     Obtain the file summary.
!        C
!              CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC,
!             .              FREE,   LASTLA, LASTRC, LASTWD       )
!
!        C
!        C     Print the summary of the new file.
!        C
!              WRITE(*,*) 'Summary before adding data:'
!              WRITE(*,*) '   Number of reserved records     :', NRESVR
!              WRITE(*,*) '   Characters in reserved records :', NRESVC
!              WRITE(*,*) '   Number of comment records      :', NCOMR
!              WRITE(*,*) '   Characters in comment area     :', NCOMC
!              WRITE(*,*) '   Number of first free record    :', FREE
!              WRITE(*,*) '   Last logical character address :',
!             .                                          LASTLA(CHARDT)
!              WRITE(*,*) '   Last logical d.p. address      :',
!             .                                          LASTLA(DPDT)
!              WRITE(*,*) '   Last logical integer address   :',
!             .                                          LASTLA(INTDT)
!              WRITE(*,*) '   Last character descriptor      :',
!             .                                          LASTRC(CHARDT)
!              WRITE(*,*) '   Last d.p descriptor            :',
!             .                                          LASTRC(DPDT)
!              WRITE(*,*) '   Last integer descriptor        :',
!             .                                          LASTRC(INTDT)
!              WRITE(*,*) '   Character word position in desc:',
!             .                                          LASTWD(CHARDT)
!              WRITE(*,*) '   d.p. word position in desc     :',
!             .                                          LASTWD(DPDT)
!              WRITE(*,*) '   Integer word position in desc  :',
!             .                                          LASTWD(INTDT)
!              WRITE(*,*)
!
!        C
!        C     Add the data.
!        C
!              DO I = 1, 10
!                 CALL DASADI ( HANDLE, 1, I )
!              END DO
!
!              DO I = 1, 5
!                 CALL DASADD ( HANDLE, 1, DBLE(I) )
!              END DO
!
!        C
!        C     Add character data to the file. DAS character data are
!        C     treated as a character array, not as a string. The
!        C     following call adds only the first 4 characters to the
!        C     DAS file.
!        C
!              CALL DASADC ( HANDLE, 4, 1, 4, 'SPUDWXY' )
!
!        C
!        C     Close the file and open it for reading.
!        C
!              CALL DASCLS ( HANDLE        )
!              CALL DASOPR ( FNAME, HANDLE )
!
!        C
!        C     Obtain again the file summary.
!        C
!              CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC,
!             .              FREE,   LASTLA, LASTRC, LASTWD       )
!
!              WRITE(*,*) 'Summary after adding data:'
!              WRITE(*,*) '   Number of reserved records     :', NRESVR
!              WRITE(*,*) '   Characters in reserved records :', NRESVC
!              WRITE(*,*) '   Number of comment records      :', NCOMR
!              WRITE(*,*) '   Characters in comment area     :', NCOMC
!              WRITE(*,*) '   Number of first free record    :', FREE
!              WRITE(*,*) '   Last logical character address :',
!             .                                          LASTLA(CHARDT)
!              WRITE(*,*) '   Last logical d.p. address      :',
!             .                                          LASTLA(DPDT)
!              WRITE(*,*) '   Last logical integer address   :',
!             .                                          LASTLA(INTDT)
!              WRITE(*,*) '   Last character descriptor      :',
!             .                                          LASTRC(CHARDT)
!              WRITE(*,*) '   Last d.p descriptor            :',
!             .                                          LASTRC(DPDT)
!              WRITE(*,*) '   Last integer descriptor        :',
!             .                                          LASTRC(INTDT)
!              WRITE(*,*) '   Character word position in desc:',
!             .                                          LASTWD(CHARDT)
!              WRITE(*,*) '   d.p. word position in desc     :',
!             .                                          LASTWD(DPDT)
!              WRITE(*,*) '   Integer word position in desc  :',
!             .                                          LASTWD(INTDT)
!              WRITE(*,*)
!
!        C
!        C     Read the integers and dump them.
!        C
!              WRITE(*,*) 'Integer data in the DAS file:'
!              DO I = 1, LASTLA(INTDT)
!                 CALL DASRDI ( HANDLE, I, I, N )
!                 WRITE (*,*) '   ', N
!              END DO
!
!        C
!        C     Now the d.p. numbers:
!        C
!              WRITE(*,*)
!              WRITE(*,*) 'Double precision data in the DAS file:'
!              DO I = 1, LASTLA(DPDT)
!                 CALL DASRDD ( HANDLE, I, I, X )
!                 WRITE (*,*) '   ', X
!              END DO
!
!        C
!        C     Now the characters. In this case, we read the
!        C     data one line at a time.
!        C
!              FIRST   =  0
!              LAST    =  0
!              REMAIN  =  LASTLA(CHARDT)
!
!              WRITE(*,*)
!              WRITE(*,*) 'Character data in the DAS file:'
!              DO WHILE ( REMAIN .GT. 0 )
!
!                 NREAD = MIN ( LINLEN, REMAIN )
!                 FIRST = LAST + 1
!                 LAST  = LAST + NREAD
!
!                 CALL DASRDC ( HANDLE, FIRST, LAST, 1, LINLEN, LINE )
!
!                 WRITE (*,*) '   ', LINE(:NREAD)
!
!                 REMAIN = REMAIN - NREAD
!
!              END DO
!
!        C
!        C     Close the file.
!        C
!              CALL DASCLS ( HANDLE )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Summary before adding data:
!            Number of reserved records     :           0
!            Characters in reserved records :           0
!            Number of comment records      :           0
!            Characters in comment area     :           0
!            Number of first free record    :           3
!            Last logical character address :           0
!            Last logical d.p. address      :           0
!            Last logical integer address   :           0
!            Last character descriptor      :           0
!            Last d.p descriptor            :           0
!            Last integer descriptor        :           0
!            Character word position in desc:           0
!            d.p. word position in desc     :           0
!            Integer word position in desc  :           0
!
!         Summary after adding data:
!            Number of reserved records     :           0
!            Characters in reserved records :           0
!            Number of comment records      :           0
!            Characters in comment area     :           0
!            Number of first free record    :           6
!            Last logical character address :           4
!            Last logical d.p. address      :           5
!            Last logical integer address   :          10
!            Last character descriptor      :           2
!            Last d.p descriptor            :           2
!            Last integer descriptor        :           2
!            Character word position in desc:          10
!            d.p. word position in desc     :          11
!            Integer word position in desc  :          12
!
!         Integer data in the DAS file:
!                       1
!                       2
!                       3
!                       4
!                       5
!                       6
!                       7
!                       8
!                       9
!                      10
!
!         Double precision data in the DAS file:
!               1.0000000000000000
!               2.0000000000000000
!               3.0000000000000000
!               4.0000000000000000
!               5.0000000000000000
!
!         Character data in the DAS file:
!            SP
!            UD
!
!
!        Note that after run completion, a new DAS file exists in the
!        output directory.
!
! $ Restrictions
!
!     None.
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 6.0.2, 19-JUL-2021 (JDR)
!
!        Updated the header to comply with NAIF standard. Added
!        complete code example.
!
!        Described the public parameters used together with this module
!        in the $Parameters section.
!
!-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if these open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 30-JUL-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     return the file summary of a DAS file
!     find the amount of data in a DAS file
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if these open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 30-JUL-1992 (NJB) (WLT)
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'DASHFS' )
      end if

!
!     Initialize the file table pool and handle list, if necessary.
!
      if ( pass1 ) then

         call lnkini ( ftsize, pool   )
         call ssizei ( ftsize, fhlist )

         pass1 = .false.

      end if


      findex  =  fthead
      found   = .false.

      do while (  ( .not. found )  .and.  ( findex .gt. 0 )  )

         if ( fthan(findex) .eq. handle ) then
            found = .true.
         else
            findex = lnknxt ( findex, pool )
         end if

      end do


      if ( found ) then
!
!        Give the caller the current summary from the file table.
!
         nresvr  =  ftsum ( rrcidx, findex )
         nresvc  =  ftsum ( rchidx, findex )
         ncomr   =  ftsum ( crcidx, findex )
         ncomc   =  ftsum ( cchidx, findex )
         free    =  ftsum ( freidx, findex )

         do i = 1, 3
            lastla(i)  =  ftsum ( llabas + i,  findex )
            lastrc(i)  =  ftsum ( lrcbas + i,  findex )
            lastwd(i)  =  ftsum ( lwdbas + i,  findex )
         end do

      else
         call setmsg ( 'There is no DAS file open with handle = #' )
         call errint ( '#', handle                                 )
         call sigerr ( 'SPICE(DASNOSUCHHANDLE)'                    )
      end if

      call chkout ( 'DASHFS' )
      return


!$Procedure DASUFS ( DAS, update file summary )

      entry dasufs ( handle, &
                     nresvr, &
                     nresvc, &
                     ncomr, &
                     ncomc, &
                     free, &
                     lastla, &
                     lastrc, &
                     lastwd )

! $ Abstract
!
!     Update the file summary in a specified DAS file.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     CONVERSION
!     DAS
!     FILES
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               NRESVR
!     INTEGER               NRESVC
!     INTEGER               NCOMR
!     INTEGER               NCOMC
!     INTEGER               FREE
!     INTEGER               LASTLA ( 3 )
!     INTEGER               LASTRC ( 3 )
!     INTEGER               LASTWD ( 3 )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of an open DAS file.
!     NRESVR     I   Number of reserved records in file.
!     NRESVC     I   Number of characters in use in reserved rec. area.
!     NCOMR      I   Number of comment records in file.
!     NCOMC      I   Number of characters in use in comment area.
!     FREE       I   Number of first free record.
!     LASTLA     I   Array of last logical addresses for each data type.
!     LASTRC     I   Record number of last descriptor of each data type.
!     LASTWD     I   Word number of last descriptor of each data type.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a previously opened DAS file.
!
!     NRESVR   is the number of reserved records in a specified DAS
!              file.
!
!     NRESVC   is the number of characters in use in the reserved
!              record area of a specified DAS file.
!
!     NCOMR    is the number of comment records in a specified DAS
!              file.
!
!     NCOMC    is the number of characters in use in the comment area
!              of a specified DAS file.
!
!     FREE     is the Fortran record number of the first free record
!              in a specified DAS file.
!
!     LASTLA   is an array containing the highest current logical
!              addresses, in the specified DAS file, of data of
!              character, double precision, and integer types, in
!              that order.
!
!     LASTRC   is an array containing the Fortran record numbers, in
!              the specified DAS file, of the directory records
!              containing the current last descriptors of clusters
!              of character, double precision, and integer data
!              records, in that order.
!
!     LASTWD   is an array containing the word positions, in the
!              specified DAS file, of the current last descriptors
!              of clusters of character, double precision, and
!              integer data records, in that order.
!
! $ Detailed_Output
!
!     None. See $Particulars for a description of the effect of this
!     routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the specified handle does not belong to any file that is
!         currently known to be open, the error SPICE(DASNOSUCHHANDLE)
!         is signaled.
!
!     2)  If the specified handle is not open for WRITE access, the
!         error SPICE(DASINVALIDACCESS) is signaled.
!
!     3)  If this routine's attempts to read the DAS file record
!         fail before an update, the error SPICE(DASREADFAIL) is
!         signaled.
!
!     4)  If the attempt to write to the DAS file record fails, the
!         error SPICE(DASWRITEFAIL) is signaled.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     The quantities NRESVR, NRESRC, NCOMR, NCOMC, FREE, LASTLA,
!     LASTRC, and LASTWD define the `state' of a DAS file, and in
!     particular the state of the directory structure of the file.
!     These quantities should normally be updated only by DAS routines.
!
!     The higher-level DAS routines that affect a DAS file's summary,
!     such as
!
!        DASADx
!        DASUDx
!        DASARR
!
!     automatically update the file summary, so there is no need for
!     the calling program to perform the update explicitly.
!
! $ Examples
!
!     1)  Update the last d.p. logical address for a DAS file, leaving
!         the rest of the file summary intact.
!
!            C
!            C     Read the file summary.
!            C
!                  CALL DASHFS ( HANDLE,
!                 .              NRESVR,
!                 .              RRESVC,
!                 .              NCOMR,
!                 .              NCOMC,
!                 .              FREE,
!                 .              LASTLA,
!                 .              LASTRC,
!                 .              LASTWD )
!
!            C
!            C     Update the d.p. component of the `last logical
!            C     address' array.
!            C
!                  LASTLA(DP) = NEWVAL
!
!                  CALL DASUFS ( HANDLE,
!                 .              NRESVR,
!                 .              RRESVC,
!                 .              NCOMR,
!                 .              NCOMC,
!                 .              FREE,
!                 .              LASTLA,
!                 .              LASTRC,
!                 .              LASTWD )
!
! $ Restrictions
!
!     None.
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     F.S. Turner        (JPL)
!
! $ Version
!
!-    SPICELIB Version 7.1.1, 19-JUL-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 7.1.0, 07-OCT-2015 (NJB)
!
!        Corrected call to ZZDDHCLS.
!
!-    SPICELIB Version 7.0.0, 30-JUL-2014 (NJB)
!
!        Now uses DAF/DAS handle manager subsystem.
!
!-    SPICELIB Version 6.1.0, 26-SEP-2005 (NJB)
!
!        Bug fix: file name is now correctly inserted into long
!        error message generated when target file is not open for
!        write access.
!
!-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 6.0.0, 15-OCT-2001 (FST) (NJB)
!
!        Bug fix: this routine now reads the file record
!        before attempting to update it. The buffered values
!        of IDWORD and IFN are no longer present.
!
!        Bug fix: missing call to CHKIN was added to an error
!        handling branch in entry point DASUFS. This call is
!        required because DASUFS uses discovery check-in.
!
!-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if these open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 30-JUL-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     update the file summary of a DAS file
!
!-&


! $ Revisions
!
!-    SPICELIB Version 6.1.0, 26-SEP-2005 (NJB)
!
!        Bug fix: file name is now correctly inserted into long
!        error message generated when target file is not open for
!        write access.
!
!-    SPICELIB Version 5.1.0, 15-OCT-2001 (NJB)
!
!        Bug fix: missing call to CHKIN was added to an error
!        handling branch in entry point DASUFS. This call is
!        required because DASUFS uses discovery check-in.
!
!-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if these open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 30-JUL-1992 (NJB) (WLT)
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'DASUFS' )

!
!     Initialize the file table pool and handle list, if necessary.
!
      if ( pass1 ) then

         call lnkini ( ftsize, pool   )
         call ssizei ( ftsize, fhlist )

         pass1 = .false.

      end if

!
!     Find the file table entries for this file.
!
      findex  =  fthead
      found   = .false.

      do while (  ( .not. found )  .and.  ( findex .gt. 0 )  )

         if ( fthan(findex) .eq. handle ) then
            found  = .true.
         else
            findex = lnknxt ( findex, pool )
         end if

      end do

      if ( found ) then
!
!        Obtain a logical unit for this file.
!
         call zzddhhlu ( handle, 'DAS', .false., number )

         if ( failed() ) then
            call chkout ( 'DASUFS' )
            return
         end if

!
!        Now check to see that HANDLE is open for write, as one has
!        no business updating a file summary for files that are
!        open for read access only.
!
         if ( ftacc(findex) .ne. write ) then

            call setmsg ( 'DAS file not open for writing. Handle = #,' &
            //            ' file = ''#''.'                             )
            call errint ( '#', handle                                  )
            call errfnm ( '#', number                                  )
            call sigerr ( 'SPICE(DASINVALIDACCESS)'                    )
            call chkout ( 'DASUFS'                                     )
            return

         end if

!
!        If any of the counts pertaining to the reserved record are or
!        the comment area were changed, we need to record the new
!        counts in the file record.  Otherwise, leave the file alone.
!
         if (      (  nresvr .ne. ftsum( rrcidx, findex )  ) &
              .or. (  nresvc .ne. ftsum( rchidx, findex )  ) &
              .or. (  ncomr  .ne. ftsum( crcidx, findex )  ) &
              .or. (  ncomc  .ne. ftsum( cchidx, findex )  )   )  then

!
!           Read the file record.
!
            read ( number, &
                   rec    = 1, &
                   iostat = iostat )  idword, &
                                      locifn, &
                                      locrrc, &
                                      locrch, &
                                      loccrc, &
                                      loccch, &
                                      locfmt, &
                                      tail

            if ( iostat .ne. 0 ) then

               call setmsg ( 'Attempt to read file record failed. ' &
               //            'File was ''#''.  Value of IOSTAT was' &
               //            ' ''#''.'                              )
               call errfnm ( '#', number                            )
               call errint ( '#', iostat                            )
               call sigerr ( 'SPICE(DASREADFAIL)'                   )
               call chkout ( 'DASUFS'                               )
               return

            end if

            write ( number, &
                    rec     =  1, &
                    iostat  =  iostat )  idword, &
                                         locifn, &
                                         nresvr, &
                                         nresvc, &
                                         ncomr, &
                                         ncomc, &
                                         locfmt, &
                                         tail

            if ( iostat .ne. 0 ) then
!
!              Try to obtain the DAS file's name.
!
               inquire ( unit   = number, &
                         name   = locdas, &
                         iostat = inqsta )

               call zzddhcls ( handle, 'DAS', .false. )

               if ( failed() ) then
                  call chkout ( 'DASUFS' )
                  return
               end if

               call setmsg ( 'Attempt to update file record failed. ' &
               //            'File was ''#''.  Value of IOSTAT was' &
               //             ' ''#''.'                                )
               call errch  ( '#', locdas                               )
               call errint ( '#', iostat                               )
               call sigerr ( 'SPICE(DASWRITEFAIL)'                     )
               call chkout ( 'DASUFS'                                  )
               return

            end if

         end if

!
!        Update the file table.
!
         ftsum ( rrcidx, findex )  =  nresvr
         ftsum ( rchidx, findex )  =  nresvc
         ftsum ( crcidx, findex )  =  ncomr
         ftsum ( cchidx, findex )  =  ncomc
         ftsum ( freidx, findex )  =  free

         do i = 1, 3
            ftsum ( llabas + i,  findex )  =  lastla(i)
            ftsum ( lrcbas + i,  findex )  =  lastrc(i)
            ftsum ( lwdbas + i,  findex )  =  lastwd(i)
         end do

      else

         call setmsg ( 'There is no file open with handle = #' )
         call errint ( '#', handle                             )
         call sigerr ( 'SPICE(DASNOSUCHHANDLE)'                )
         call chkout ( 'DASUFS'                                )
         return

      end if

      call chkout ( 'DASUFS' )
      return



!$Procedure DASHLU ( DAS, handle to logical unit )

      entry dashlu ( handle, unit )

! $ Abstract
!
!     Return the logical unit associated with a handle. The unit
!     is "locked" to the handle by the DAF/DAS handle manager
!     subsystem.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     CONVERSION
!     DAS
!     FILES
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               UNIT
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of a DAS file.
!     UNIT       O   Corresponding logical unit.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a previously opened DAS file.
!
! $ Detailed_Output
!
!     UNIT     is the Fortran logical unit to which the file is
!              connected.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the specified handle does not belong to any file that is
!         currently known to be open, the error SPICE(DASNOSUCHHANDLE)
!         is signaled.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     This routine is a utility used by the DAS system to support
!     file I/O. DASHLU may also prove useful to general SPICELIB
!     users for constructing error messages.
!
! $ Examples
!
!     1)  Obtain the logical unit associated with a DAS file having
!         a known handle.
!
!            CALL DASHLU ( HANDLE, UNIT )
!
! $ Restrictions
!
!     1)  Successfully invoking this module has the side effect of
!         locking UNIT to HANDLE. This 'lock' guarantees until
!         HANDLE is closed (or unlocked) that the file associated
!         with HANDLE is always open and attached to logical unit
!         UNIT. To unlock a handle without closing the file, use
!         ZZDDHUNL, an entry point in the handle manager umbrella,
!         ZZDDHMAN.
!
!         The system can lock at most UTSIZE-SCRUNT-RSVUNT
!         simultaneously (see the include file 'zzddhman.inc' for
!         specific values of these parameters), but unnecessarily
!         locking handles to their logical units may cause performance
!         degradation. The handle manager will have fewer logical
!         units to utilize when disconnecting and reconnecting
!         loaded files.
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     F.S. Turner        (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 7.0.1, 19-JUL-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 7.0.0, 04-FEB-2015 (NJB) (FST)
!
!        Now uses DAF/DAS handle manager subsystem.
!        Note that this routine is now considered obsolete.
!
!-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if these open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     map DAS file handle to logical unit
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if these open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
!
!-&


!
!     We use discovery check-ins in this routine.
!
      if ( return () ) then
         return
      end if

!
!     Initialize the file table pool and handle list, if necessary.
!
      if ( pass1 ) then

         call chkin  ( 'DASHLU'       )

         call lnkini ( ftsize, pool   )
         call ssizei ( ftsize, fhlist )

         call chkout ( 'DASHLU'       )

         pass1 = .false.

      end if

!
!     Find the file table entries for this file.
!
      findex  =  fthead
      found   = .false.

      do while (  ( .not. found )  .and.  ( findex .gt. 0 )  )

         if ( fthan(findex) .eq. handle ) then
            found = .true.
         else
            findex = lnknxt ( findex, pool )
         end if

      end do


      if ( found ) then
!
!        For backward compatibility, the logical unit must be
!        locked to the file.
!
         call zzddhhlu ( handle, 'DAS', .true., unit )

      else

         call chkin  ( 'DASHLU'                                )
         call setmsg ( 'There is no file open with handle = #' )
         call errint ( '#', handle                             )
         call sigerr ( 'SPICE(DASNOSUCHHANDLE)'                )
         call chkout ( 'DASHLU'                                )

      end if

      return



!$Procedure DASLUH ( DAS, logical unit to handle )

      entry dasluh ( unit, handle )

! $ Abstract
!
!     Return the handle associated with a logical unit.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     CONVERSION
!     DAS
!     FILES
!
! $ Declarations
!
!     INTEGER               UNIT
!     INTEGER               HANDLE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UNIT       I   Logical unit connected to a DAS file.
!     HANDLE     O   Corresponding handle.
!
! $ Detailed_Input
!
!     UNIT     is the logical unit to which a DAS file has been
!              connected when it was opened.
!
! $ Detailed_Output
!
!     HANDLE   is the handle associated with the file.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the specified unit is not connected to any DAS file that is
!         currently known to be open, the error SPICE(DASNOSUCHUNIT)
!         is signaled.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     It is unlikely, but possible, that a calling program would know
!     the logical unit to which a file is connected without knowing the
!     handle associated with the file. DASLUH is provided mostly for
!     completeness.
!
! $ Examples
!
!     In the following code fragment, the handle associated with
!     a DAS file is retrieved using the logical unit to which the
!     file is connected. The handle is then used to determine the
!     name of the file.
!
!        CALL DASLUH ( UNIT,   HANDLE )
!        CALL DASHFN ( HANDLE, FNAME  )
!
! $ Restrictions
!
!     None.
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 7.0.1, 19-JUL-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 7.0.0, 30-JUL-2014 (NJB)
!
!        Now uses DAF/DAS handle manager subsystem.
!        Note that this routine is now considered obsolete.
!
!-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if these open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     map logical unit to DAS file handle
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if these open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'DASLUH' )

!
!     Initialize the file table pool and handle list, if necessary.
!
      if ( pass1 ) then

         call lnkini ( ftsize, pool   )
         call ssizei ( ftsize, fhlist )

         pass1 = .false.

      end if

!
!     Try to locate the handle associated with this unit.
!
      call zzddhluh ( unit, handle, found )

      if ( .not. found ) then

         call setmsg ( 'There is no DAS file open with unit = #' )
         call errint ( '#', unit                                 )
         call sigerr ( 'SPICE(DASNOSUCHUNIT)'                    )

      end if

      call chkout ( 'DASLUH' )
      return



!$Procedure DASHFN ( DAS, handle to file name )

      entry dashfn ( handle, fname )

! $ Abstract
!
!     Return the name of the DAS file associated with a handle.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     CONVERSION
!     DAS
!     FILES
!
! $ Declarations
!
!     INTEGER               HANDLE
!     CHARACTER*(*)         FNAME
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of a DAS file.
!     FNAME      O   Corresponding file name.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a previously opened DAS file.
!
! $ Detailed_Output
!
!     FNAME    is the name of the DAS file associated with the input
!              file handle.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the specified handle does not belong to any file that is
!         currently known to be open, the error SPICE(DASNOSUCHHANDLE)
!         is signaled.
!
!     2)  If FNAME string is too short to contain the output string,
!         the name is truncated on the right.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     It may be desirable to recover the names of one or more DAS
!     files in a different part of the program from the one in which
!     they were opened. Note that the names returned by DASHFN may
!     not be identical to the names used to open the files. Under
!     most operating systems, a particular file can be accessed using
!     many different names. DASHFN returns one of them.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) In the following program, the name of a DAS file is
!        recovered using the handle associated with the file.
!
!
!        Use the DSK kernel below as input DAS file for the example.
!
!           phobos512.bds
!
!
!        Example code begins here.
!
!
!              PROGRAM DASHFN_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              INTEGER               RTRIM
!
!        C
!        C     Local constants
!        C
!              CHARACTER*(*)         DAS
!              PARAMETER           ( DAS    = 'phobos512.bds' )
!
!              INTEGER               FILSIZ
!              PARAMETER           ( FILSIZ = 256 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(FILSIZ)    FNAME
!
!              INTEGER               HANDLE
!
!        C
!        C     Open the DAS file for read access.
!        C
!              CALL DASOPR ( DAS, HANDLE )
!
!        C
!        C     Map the handle to a file name.
!        C
!              CALL DASHFN ( HANDLE, FNAME )
!
!              WRITE(*,*) 'DAS file name = <', FNAME(:RTRIM(FNAME)),
!             .           '>.'
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         DAS file name = <phobos512.bds>.
!
!
! $ Restrictions
!
!     None.
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 7.0.1, 19-JUL-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example. Extended $Exceptions section to describe what
!        happens in case of output string being too short.
!
!-    SPICELIB Version 7.0.0, 30-JUL-2014 (NJB)
!
!        Now uses DAF/DAS handle manager subsystem.
!        Note that this routine is now considered obsolete.
!
!-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if these open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     map DAS handle to file name
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if these open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'DASHFN' )
      end if

!
!     Initialize the file table pool and handle list, if necessary.
!
      if ( pass1 ) then

         call lnkini ( ftsize, pool   )
         call ssizei ( ftsize, fhlist )

         pass1 = .false.

      end if

!
!     Find the file table entries for this file.
!
      findex  =  fthead
      found   = .false.

      do while (  ( .not. found )  .and.  ( findex .gt. 0 )  )

         if ( fthan(findex) .eq. handle ) then
            found  = .true.
         else
            findex = lnknxt ( findex, pool )
         end if

      end do

      if ( found ) then

         call zzddhhlu ( handle, 'DAS', .false., number )

         inquire ( unit = number, &
                   name = fname  )

      else

         call setmsg ( 'There is no DAS file open with handle = #' )
         call errint ( '#', handle                                 )
         call sigerr ( 'SPICE(DASNOSUCHHANDLE)'                    )

      end if

      call chkout ( 'DASHFN' )
      return


!$Procedure DASFNH ( DAS, file name to handle )

      entry dasfnh ( fname, handle )

! $ Abstract
!
!     Return handle associated with a file name.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     CONVERSION
!     DAS
!     FILES
!
! $ Declarations
!
!     CHARACTER*(*)         FNAME
!     INTEGER               HANDLE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FNAME      I   Name of a DAS file.
!     HANDLE     O   Corresponding handle.
!
! $ Detailed_Input
!
!     FNAME    is the name of a previously opened DAS file.
!
! $ Detailed_Output
!
!     HANDLE   is the handle associated with the file.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the specified name does not specify any DAS file currently
!         known to be open, the error SPICE(DASNOSUCHFILE) is signaled.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     It is sometimes easier to work with file names (which are
!     meaningful, and often predictable) than with file handles
!     (which are neither), especially in interactive situations.
!     However, nearly every DAS routine requires that you use file
!     handles to refer to files. DASFNH is provided to bridge the gap
!     between the two representations.
!
! $ Examples
!
!     In the following code fragment, the handle associated with a
!     DAS file is recovered using the name of the file.
!
!        CALL DASOPR ( 'sample.DAS', HANDLE )
!         .
!         .
!
!        CALL DASFNH ( 'sample.DAS', HANDLE )
!
! $ Restrictions
!
!     None.
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 7.0.1, 19-JUL-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 7.0.0, 30-JUL-2014 (NJB)
!
!        Now uses DAF/DAS handle manager subsystem.
!        Note that this routine is now considered obsolete.
!
!-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if these open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     map file name to DAS handle
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if these open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'DASFNH' )

!
!     Initialize the file table pool and handle list, if necessary.
!
      if ( pass1 ) then

         call lnkini ( ftsize, pool   )
         call ssizei ( ftsize, fhlist )

         pass1 = .false.

      end if

      call zzddhfnh ( fname, handle, found )

      if ( .not. found ) then

         call setmsg ( 'There is no DAS file in the table with file ' // &
                       'name = ''#'''                                  )
         call errch  ( '#', fname                                      )
         call sigerr ( 'SPICE(DASNOSUCHFILE)'                          )

      end if

      call chkout ( 'DASFNH' )
      return


!$Procedure DASHOF ( DAS, handles of open files )

      entry dashof ( fhset )

! $ Abstract
!
!     Return a SPICE set containing the handles of all currently
!     open DAS files.
!
! $ Required_Reading
!
!     DAS
!     SETS
!
! $ Keywords
!
!     DAS
!     FILES
!
! $ Declarations
!
!     INTEGER               LBCELL
!     PARAMETER           ( LBCELL = -5 )
!
!     INTEGER               FHSET ( LBCELL : * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FHSET      O   A set containing handles of currently open DAS
!                    files.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     FHSET    is a SPICE set containing the file handles of
!              all currently open DAS files.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the set FHSET is not initialized, an error is signaled by a
!         routine in the call tree of this routine.
!
!     2)  If the set FHSET is too small to accommodate the set of
!         handles to be returned, an error is signaled by a routine in
!         the call tree of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine allows subroutines to test DAS file handles for
!     validity before using them. Many DAS operations that rely on
!     handles to identify DAS files cause errors to be signaled if
!     the handles are invalid.
!
! $ Examples
!
!     1)  Find out how may DAS files are open for writing.
!
!            C
!            C    Find out which DAS files are open.
!            C
!                 CALL DASHOF  ( FHSET )
!
!            C
!            C    Count the ones open for writing.
!            C
!                 COUNT = 0
!
!                 DO I = 1, CARDC(FHSET)
!
!                    CALL DASHAM ( FHSET(I), METHOD )
!
!                    IF ( METHOD .EQ. WRITE ) THEN
!                       COUNT = COUNT + 1
!                    END IF
!
!                 END DO
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 6.0.2, 19-JUL-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     return set of handles of open DAS files
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'DASHOF' )
      end if

!
!     Initialize the file table pool and handle list, if necessary.
!
      if ( pass1 ) then

         call lnkini ( ftsize, pool   )
         call ssizei ( ftsize, fhlist )

         pass1 = .false.

      end if

!
!     Just stuff our local list into the set.
!
      call copyi ( fhlist, fhset )

      call chkout ( 'DASHOF' )
      return



!$Procedure DASSIH ( DAS, signal invalid handles )

      entry dassih ( handle, access )

! $ Abstract
!
!     Signal an error if a DAS file file handle does not designate a
!     DAS file that is open for a specified type of access.
!
! $ Required_Reading
!
!     DAS
!     ERROR
!     SETS
!
! $ Keywords
!
!     DAS
!     FILES
!     UTILITY
!
! $ Declarations
!
!     INTEGER               HANDLE
!     CHARACTER*(*)         ACCESS
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   HANDLE to be validated.
!     ACCESS     I   String indicating access type.
!
! $ Detailed_Input
!
!     HANDLE   is a DAS file handle to validate. For HANDLE to be
!              considered valid, it must specify a DAS file that
!              is open for the type of access specified by the
!              input argument ACCESS.
!
!
!     ACCESS   is a string indicating the type of access that
!              the DAS file specified by the input argument HANDLE
!              must be open for. The values of ACCESS may be
!
!                 'READ'      File must be open for read access
!                             by DAS routines. DAS files opened
!                             for read or write access may be
!                             read.
!
!                 'WRITE'     File must be open for write access
!                             by DAS routines. Note that files
!                             open for write access may be read as
!                             well as written.
!
!              Leading and trailing blanks in ACCESS are ignored,
!              and case is not significant.
!
! $ Detailed_Output
!
!     None. See $Particulars for a description of the effect of this
!     routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input argument ACCESS has an unrecognized value,
!         the error SPICE(INVALIDOPTION) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine signals the error SPICE(DASINVALIDACCESS) if the
!     DAS designated by the input argument HANDLE is not open
!     for the specified type of access. If HANDLE does not designate
!     an open DAS file, the error SPICE(DASNOSUCHHANDLE) is signaled.
!
!     This routine allows subroutines to test file handles for
!     validity before attempting to access the files they designate,
!     or before performing operations on the handles themselves, such
!     as finding the name of the file designated by a handle. This
!     routine should be used in situations where the appropriate action
!     to take upon determining that a handle is invalid is to signal
!     an error. DASSIH centralizes the error response for this type of
!     error in a single routine.
!
!     In cases where it is necessary to determine the validity of a
!     file handle, but it is not an error for the handle to refer
!     to a closed file, the entry point DASHOF should be used instead
!     of DASSIH.
!
! $ Examples
!
!     1)  Make sure that a file handle designates a DAS file that can
!         be read. Signal an error if not.
!
!         Note that if a DAS file is open for reading or writing, read
!         access is allowed.
!
!                  CALL DASSIH ( HANDLE, 'READ' )
!
!                  IF ( FAILED() ) THEN
!                     RETURN
!                  END IF
!
! $ Restrictions
!
!     None.
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 6.1.1, 19-JUL-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 6.1.0, 26-SEP-2005 (NJB)
!
!        Local variable DAS was renamed to DASFIL. This
!        was done to avoid future conflict with parameters
!        in zzddhman.inc.
!
!-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if these open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     detect invalid DAS handles
!     validate DAS handles
!
!-&


! $ Revisions
!
!-    SPICELIB Version 6.1.0, 26-SEP-2005 (NJB)
!
!        Local variable DAS was renamed to DASFIL. This
!        was done to avoid future conflict with parameters
!        in zzddhman.inc.
!
!-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if these open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'DASSIH' )

!
!     Initialize the file table pool and handle list, if necessary.
!
      if ( pass1 ) then

         call lnkini ( ftsize, pool   )
         call ssizei ( ftsize, fhlist )

         pass1 = .false.

      end if

!
!     Get an upper case, left-justified copy of ACCESS.
!
      call ljust ( access, acc )
      call ucase ( acc,    acc )

!
!     Make sure we recognize the access type specified by the caller.
!
      if (  ( acc .ne. 'READ' ) .and. ( acc .ne. 'WRITE' )  ) then

         call setmsg ( 'Unrecognized access type.  Type was #. ' )
         call errch  ( '#', access                               )
         call sigerr ( 'SPICE(INVALIDOPTION)'                    )
         call chkout ( 'DASSIH'                                  )
         return

      end if

!
!     See whether the input handle is in our list at all.  It's
!     unlawful for the handle to be absent.
!
      if (  .not.  elemi ( handle, fhlist )  )  then

         call setmsg ( 'Handle # is not attached to an open DAS file.' )
         call errint ( '#', handle                                     )
         call sigerr ( 'SPICE(DASNOSUCHHANDLE)'                        )
         call chkout ( 'DASSIH'                                        )
         return

      else
!
!        Find the file table entries for this file.  We know they
!        must exist.
!
         findex  =  fthead
         found   = .false.

         do while (  ( .not. found )  .and.  ( findex .gt. 0 )  )

            if ( fthan(findex) .eq. handle ) then
               found  = .true.
            else
               findex = lnknxt ( findex, pool )
            end if

         end do
!
!        At this point, FINDEX points to the file table entries
!        for this file.
!

         if (       ( acc           .eq. 'WRITE' ) &
              .and. ( ftacc(findex) .ne.  write  )  ) then
!
!           If the access type is 'WRITE', the DAS file must be open
!           for writing.
!
            call zzddhhlu ( handle, 'DAS', .false., number )

            call setmsg ( 'DAS file not open for writing. Handle = #,'// &
                          ' file = ''#''.'                             )
            call errint ( '#', handle                                  )
            call errfnm ( '#', number                                  )
            call sigerr ( 'SPICE(DASINVALIDACCESS)'                    )
            call chkout ( 'DASSIH'                                     )
            return

         end if

      end if

!
!     The DAS file's handle is o.k.
!
      call chkout ( 'DASSIH' )
      return


!$Procedure DASHAM ( DAS, handle to access method )

      entry dasham ( handle, access )

! $ Abstract
!
!     Return the allowed access method for a specified DAS file.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     DAS
!     FILES
!     UTILITY
!
! $ Declarations
!
!     INTEGER               HANDLE
!     CHARACTER*(*)         ACCESS
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   HANDLE of a DAS file.
!     ACCESS     O   String indicating allowed access method.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a previously opened DAS file.
!
! $ Detailed_Output
!
!     ACCESS   is a string indicating the type of access that
!              the DAS file specified by the input argument HANDLE
!              is open for. The values of ACCESS may be
!
!                 'READ'      File is open for read access by DAS
!                             routines. Both the data area and
!                             the comment area may be read. The
!                             file may not be modified.
!
!                 'WRITE'     File is open for write access by
!                             DAS routines. Files open for
!                             write access may be read as well as
!                             written.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input handle is invalid, the error SPICE(INVALIDHANDLE)
!         is signaled. ACCESS is not modified.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine allows subroutines to determine the access methods
!     allowed for a given DAS file.
!
! $ Examples
!
!     1)  Make sure that a file handle designates a DAS file that can
!         be read. Signal an error if not.
!
!         Note that if a DAS file is open for reading or writing, read
!         access is allowed.
!
!                  CALL DASHAM ( HANDLE, 'READ' )
!
!                  IF ( FAILED() ) THEN
!                     RETURN
!                  END IF
!
! $ Restrictions
!
!     None.
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 6.0.2, 19-JUL-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input and $ Output sections of the header. This was
!        done in order to minimize documentation changes if these open
!        routines ever change.
!
!-    SPICELIB Version 1.0.0, 01-FEB-1993 (NJB) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     return allowed access methods for DAS files
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input and $ Output sections of the header. This was
!        done in order to minimize documentation changes if these open
!        routines ever change.
!
!-    SPICELIB Version 1.0.0, 01-FEB-1993 (NJB) (WLT) (IMU)
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'DASHAM' )

!
!     Initialize the file table pool and handle list, if necessary.
!
      if ( pass1 ) then

         call lnkini ( ftsize, pool   )
         call ssizei ( ftsize, fhlist )

         pass1 = .false.

      end if

!
!     See whether the input handle is in our list at all.  It's
!     unlawful for the handle to be absent.
!
      findex  =  fthead
      found   = .false.

      do while (  ( .not. found )  .and.  ( findex .gt. 0 )  )

         if ( fthan(findex) .eq. handle ) then
            found  = .true.
         else
            findex = lnknxt ( findex, pool )
         end if

      end do

      if ( .not. found ) then

         call setmsg ( 'The handle # does not designate a known DAS ' // &
                       'file '                                         )
         call errint ( '#',  handle                                    )
         call sigerr ( 'SPICE(INVALIDHANDLE)'                          )
         call chkout ( 'DASHAM'                                        )
         return

      end if

!
!     We know about the file if we got this far.  Set the output
!     argument accordingly.
!
      if (  ftacc(findex) .eq. read ) then
         access = 'READ'
      else
         access = 'WRITE'
      end if

      call chkout ( 'DASHAM' )
      return
      end

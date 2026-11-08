!$Procedure KEEPER ( Keeps track of SPICE kernels )

      SUBROUTINE KEEPER ( WHICH,  KIND, &
                          FILE,   COUNT, FILTYP,  HANDLE, &
                          SRCFIL, FOUND )

! $ Abstract
!
!     This routine is an umbrella for a collection of entry points
!     that manage the loading and unloading of SPICE kernels from
!     an application program.
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
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               WHICH
      CHARACTER*(*)         KIND
      CHARACTER*(*)         FILE
      INTEGER               COUNT
      CHARACTER*(*)         FILTYP
      INTEGER               HANDLE
      CHARACTER*(*)         SRCFIL
      LOGICAL               FOUND


      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               FTSIZE
      PARAMETER           ( FTSIZE = 5000 )

      INTEGER               MAXTXT
      PARAMETER           ( MAXTXT = 300 )

      INTEGER               MAXFIL
      PARAMETER           ( MAXFIL = FTSIZE + MAXTXT )

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     WHICH      I   KDATA
!     KIND       I   KTOTAL, KDATA
!     FILE      I-O  FURNSH, KDATA, UNLOAD, KINFO
!     COUNT      O   KTOTAL
!     FILTYP    I-O  KTOTAL, KDATA, KINFO
!     HANDLE     O   KDATA, KINFO
!     SRCFIL     O   KDATA, KINFO
!     FOUND      O   KDATA, KINFO
!     FILSIZ     P   Maximum file name length.
!     MAXFIL     P   Maximum number of files that can be loaded.
!
! $ Detailed_Input
!
!     See Individual Entry points.
!
! $ Detailed_Output
!
!     See Individual Entry points.
!
! $ Parameters
!
!     FILSIZ   is the maximum file name length that can be
!              accommodated by this set of routines.
!
!
!     MAXFIL   is the number of entries that can be stored in KEEPER's
!              kernel database. In this version of the toolkit MAXFIL
!              is set to 5300. Each time a kernel is loaded via
!              FURNSH, a database entry is created for that kernel.
!              If a meta-kernel is loaded, a database entry is created
!              for the meta-kernel itself and for all files referenced
!              in the meta-kernel's KERNELS_TO_LOAD specification.
!              Unloading a kernel or meta-kernel deletes database
!              entries created when the file was loaded.
!
!              The parameter MAXFIL is an upper bound on number of
!              SPICE kernels that can be loaded at any time via the
!              KEEPER interface, but the number of kernels that can be
!              loaded may be smaller, since re-loading a loaded kernel
!              or meta-kernel results in creation of additional
!              database entries.
!
!              Kernels loaded into the KEEPER system are subject to
!              constraints imposed by lower-level subsystems. The
!              binary kernel systems (SPK, CK, binary PCK, and EK)
!              have their own limits on the maximum number of kernels
!              that may be loaded.
!
!              The total number of DAF-based files (this set includes
!              SPKs, CKs, and binary PCKs) and DAS-based files (this
!              set includes EKs and DSKs) that may be loaded at any
!              time may not exceed 5000. This limit applies whether
!              the files are loaded via FURNSH or lower-level loaders
!              such as SPKLEF or DAFOPR. File access performance
!              normally will degrade as the number of loaded kernels
!              increases.
!
! $ Exceptions
!
!     1)  If the main routine KEEPER is called, the error
!         SPICE(BOGUSENTRY) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine serves as an umbrella for a collection of
!     entry points that unify the task of loading, tracking,
!     and unloading SPICE kernels. A description of each entry
!     point is given below:
!
!     FURNSH    Furnish a kernel to a program. This entry point
!               provides a single interface for loading kernels into
!               your application program. All SPICE kernels (Text
!               kernels, SPK, CK, Binary PCK, and EK) can be loaded
!               through this entry point. In addition, special text
!               kernels, called meta-Text kernels, that contain a list
!               of other kernels to load can be processed by FURNSH.
!
!               Meta-text kernels allow you to easily control which
!               kernels will be loaded by your program without having
!               to write your own kernel managing routines.
!
!     KTOTAL    returns the number of kernels that are currently
!               available to your program as a result of previous calls
!               to FURNSH and UNLOAD.
!
!     KDATA     provides an interface for retrieving (in order of their
!               specification through FURNSH) kernels that are active in
!               your application.
!
!     KINFO     allows you to retrieve information about a loaded
!               kernel using the name of that kernel.
!
!     KCLEAR    Unloads all kernels that were loaded via the KEEPER
!               system, clears the kernel pool, and re-initializes the
!               KEEPER system.
!
!     UNLOAD    provides an interface for unloading kernels that have
!               been loaded via the routine FURNSH.
!
!     For more details concerning any particular entry point, see the
!     header for that entry point.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Load a meta-kernel with a PCK, an LSK and an SPK, and
!        separately, a text kernel and a binary PCK. Loop over the
!        loaded kernels, outputting file information for each of
!        them.
!
!        Then unload the text kernels, check that they have been
!        unloaded, and finally unload all remaining kernels
!        and clear the kernel pool using KCLEAR.
!
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: keeper_ex1.tm
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
!              File name                     Contents
!              ---------                     --------
!              de421.bsp                     Planetary ephemeris
!              pck00009.tpc                  Planet orientation and
!                                            radii
!              naif0012.tls                  Leapseconds
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'de421.bsp',
!                                  'naif0012.tls',
!                                  'pck00009.tpc' )
!
!           \begintext
!
!           End of meta-kernel
!
!
!        Use the PCK kernel below as the binary PCK required for the
!        example.
!
!           earth_latest_high_prec.bpc
!
!
!        Use the FK kernel below as the text kernel required for the
!        example.
!
!           RSSD0002.TF
!
!
!        Example code begins here.
!
!
!              PROGRAM KEEPER_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local constants.
!        C
!              INTEGER               FNAMLN
!              PARAMETER           ( FNAMLN = 256 )
!
!              INTEGER               FTYPLN
!              PARAMETER           ( FTYPLN = 33 )
!
!              INTEGER               SRCLEN
!              PARAMETER           ( SRCLEN = 256 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(FNAMLN)    FILE
!              CHARACTER*(FTYPLN)    FILTYP
!              CHARACTER*(SRCLEN)    SRCFIL
!
!              INTEGER               COUNT
!              INTEGER               HANDLE
!              INTEGER               WHICH
!
!              LOGICAL               FOUND
!
!        C
!        C     Load several kernel files.
!        C
!              CALL FURNSH ( 'keeper_ex1.tm'              )
!              CALL FURNSH ( 'RSSD0002.TF'                )
!              CALL FURNSH ( 'earth_latest_high_prec.bpc' )
!
!        C
!        C     Count the number of loaded kernel files.
!        C
!              CALL KTOTAL ( 'ALL', COUNT )
!
!              WRITE(*,'(A,I2)') 'The total number of kernels after '
!             . //               'final FURNSH: ', COUNT
!              WRITE(*,*) ' '
!
!        C
!        C     Retrieve the data for all the loaded kernels and
!        C     test an index for which there should be no kernel.
!        C
!              WRITE(*,'(A)') 'Overview of loaded kernels, by index:'
!              WRITE(*,*) ' '
!
!              DO WHICH= 1, COUNT + 1
!
!                 CALL KDATA ( WHICH, 'ALL',   FILE, FILTYP,
!             .                SRCFIL, HANDLE, FOUND        )
!
!                 IF ( FOUND ) THEN
!
!                    WRITE(*,*) '  Index : ', WHICH
!                    WRITE(*,*) '  File  : ', FILE
!                    WRITE(*,*) '  Type  : ', FILTYP
!                    WRITE(*,*) '  Source: ', SRCFIL
!                    WRITE(*,*) '  Handle: ', HANDLE
!                    WRITE(*,*) ' '
!
!                 ELSE
!
!                    WRITE(*,*) '  No kernel found with index: ', WHICH
!
!                 END IF
!
!              END DO
!
!        C
!        C     Unload the text kernels.
!        C
!              CALL KTOTAL ( 'TEXT', COUNT )
!
!              WRITE(*,*) ' '
!              WRITE(*,'(A,I2,A)') 'Unloading', COUNT,
!             .                    ' text kernels...'
!              WRITE(*,*) ' '
!
!              DO WHILE ( COUNT .NE. 0 )
!
!                 CALL KDATA (      1, 'TEXT',  FILE, FILTYP,
!             .                SRCFIL, HANDLE, FOUND        )
!
!        C
!        C        If the kernel is found in the pool, unload it.
!        C
!                 IF ( FOUND ) THEN
!
!                    CALL UNLOAD ( FILE )
!
!        C
!        C           Check if the file has been unloaded.
!        C
!                    CALL KINFO ( FILE, FILTYP, SRCFIL, HANDLE, FOUND )
!
!                    IF ( FOUND ) THEN
!
!                       WRITE(*,'(A)') '  Error unloading ' // FILE
!
!                    ELSE
!
!                       WRITE(*,'(A)') '  Success unloading ' // FILE
!
!                    END IF
!
!        C
!        C        Something is not working. Inform NAIF.
!        C
!                 ELSE
!
!                    WRITE(*,*) ' ERROR: No kernel found with index: ',
!             .                 WHICH
!
!                 END IF
!
!        C
!        C        Check if we have more text kernels to unload from
!        C        the kernel pool. Note that unloading a text kernel
!        C        or meta-kernel implies that the kernel pool is
!        C        cleared, and any kernel(s) that were not to be
!        C        unloaded are re-loaded. Therefore the COUNT value
!        C        changes, and the indexing of the files within the
!        C        kernel pool too.
!        C
!                 CALL KTOTAL ( 'TEXT', COUNT )
!
!              END DO
!
!              CALL KTOTAL ( 'ALL', COUNT )
!
!              WRITE(*,*) ' '
!              WRITE(*,'(A,I2)') 'The total number of kernels after '
!             . //               'UNLOAD calls: ', COUNT
!
!        C
!        C     Clear the KEEPER system, retrieve the number of loaded
!        C     after the clear.
!        C
!              CALL KCLEAR()
!
!              CALL KTOTAL ( 'ALL', COUNT )
!
!              WRITE(*,*) ' '
!              WRITE(*,'(A,I2)') 'The total number of kernels after '
!             . //               'KCLEAR      : ', COUNT
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        The total number of kernels after final FURNSH:  6
!
!        Overview of loaded kernels, by index:
!
!           Index :            1
!           File  : keeper_ex1.tm
!           Type  : META
!           Source:
!           Handle:            0
!
!           Index :            2
!           File  : de421.bsp
!           Type  : SPK
!           Source: keeper_ex1.tm
!           Handle:            1
!
!           Index :            3
!           File  : naif0012.tls
!           Type  : TEXT
!           Source: keeper_ex1.tm
!           Handle:            0
!
!           Index :            4
!           File  : pck00009.tpc
!           Type  : TEXT
!           Source: keeper_ex1.tm
!           Handle:            0
!
!           Index :            5
!           File  : RSSD0002.TF
!           Type  : TEXT
!           Source:
!           Handle:            0
!
!           Index :            6
!           File  : earth_latest_high_prec.bpc
!           Type  : PCK
!           Source:
!           Handle:            2
!
!           No kernel found with index:            7
!
!        Unloading 3 text kernels...
!
!          Success unloading naif0012.tls
!          Success unloading pck00009.tpc
!          Success unloading RSSD0002.TF
!
!        The total number of kernels after UNLOAD calls:  3
!
!        The total number of kernels after KCLEAR      :  0
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
!     C.H. Acton         (JPL)
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 5.1.0, 29-DEC-2021 (JDR) (NJB)
!
!        Changed argument name SOURCE to SRCFIL for consistency with
!        other routines.
!
!        Updated KEEPER umbrella routine and all entry points' headers
!        to comply with NAIF standard.
!
!        Updated description of input argument KIND in headers of entry
!        points KDATA and KTOTAL. Updated $Brief_I/O table to include
!        WHICH short description and sort arguments in the order they
!        are declared.
!
!        Added a restriction about specifying kernels using relative
!        paths to the FURNSH entry point header $Restrictions section.
!
!-    SPICELIB Version 5.0.0, 01-FEB-2017 (NJB) (BVS)
!
!        Updated to support use of DSKs.
!
!        Bug fix: now unloads binary kernels via low-level
!        unload routines only when those kernels have just
!        one entry in the KEEPER database.
!
!        Updated description of MAXFIL in the header.
!
!        Updated the $Author_and_Institution section.
!
!-    SPICELIB Version 4.1.0, 01-JUL-2014 (NJB) (BVS)
!
!        Updated the discussion of kernel variable watches in entry
!        points KCLEAR and UNLOAD. Added to the FURNSH header mention
!        of the effects of failure during text kernel or meta-kernel
!        loading.
!
!        Last update was 12-APR-2012 (BVS)
!
!           Increased FTSIZE (from 1000 to 5000).
!
!           Changed to use SEPOOL instead of STPOOL to reduce loading
!           time for large meta-kernels due to n^2 delay in STPOOL.
!
!-    SPICELIB Version 4.0.2, 13-APR-2011 (EDW)
!
!        Trivial edit to KCLEAR $Restrictions, replaced P*POOL with
!        PXPOOL. The "*" character causes the HTML documentation
!        script to create a link for the "POOL" substring.
!
!-    SPICELIB Version 4.0.1, 10-FEB-2010 (EDW)
!
!        Added mention of the restriction on kernel pool variable
!        names to MAXLEN (defined in pool.f) characters or less.
!
!-    SPICELIB Version 4.0.0, 02-APR-2009 (NJB)
!
!        Continued path values are now supported. FURNSH now rejects
!        file names longer than FILSIZ characters.
!
!        Deleted references to unneeded variable DOALL. Made
!        THSTYP declaration compatible with TYPES array.
!
!-    SPICELIB Version 3.0.1, 27-APR-2007 (NJB)
!
!        Fixed header typo: added quotes to literal string
!        input arguments in example FURNSH calls.
!
!-    SPICELIB Version 3.0.0, 15-NOV-2006 (NJB)
!
!        Added entry point KCLEAR. Bug fix: meta-kernel unloading bug
!        in UNLOAD was corrected. Some header updates were made.
!
!-    SPICELIB Version 2.0.2, 29-JUL-2003 (NJB) (CHA)
!
!        Only the header of the entry point FURNSH was modified.
!        Numerous updates were made to improve clarity. Some
!        corrections were made.
!
!-    SPICELIB Version 2.0.1, 06-DEC-2002 (NJB)
!
!        Typo in header example was corrected.
!
!-    SPICELIB Version 2.0.0, 07-JAN-2002 (WLT)
!
!        Added a call to CVPOOL in FURNSH so that watches that are
!        triggered are triggered by loading Meta-kernels and not by
!        some external interaction with the kernel pool.
!
!        Added code to make sure that UNLOAD has the effect of
!        loading all remaining kernels in the order they were first
!        introduced.
!
!-    SPICELIB Version 1.1.0, 19-SEP-2000 (WLT)
!
!        Corrected the error message template used
!        by ZZLDKER
!
!-    SPICELIB Version 1.0.1, 16-DEC-1999 (NJB)
!
!        Documentation fix: corrected second code example in the
!        header of the entry point FURNSH. The example previously used
!        the kernel variable PATH_NAMES; that name has been replaced
!        with the correct name PATH_VALUES.
!
!-    SPICELIB Version 1.0.0, 01-JUL-1999 (WLT)
!
!-&


! $ Index_Entries
!
!     Generic loading and unloading of SPICE kernels
!
!-&


!
!     SPICELIB Functions
!
      INTEGER               ISRCHC
      INTEGER               POS
      INTEGER               RTRIM

      LOGICAL               EQSTR
      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               SAMSUB

!
!     Here we set up the database of loaded kernels
!
!     The name of every file loaded through this interface will
!     be stored in the array FILES.
!
      CHARACTER*(FILSIZ)    FILES ( MAXFIL )
      SAVE                  FILES
!
!     The handle of every loaded file will be stored in the array
!     HANDLS.  If the file is a text kernel it will be assigned the
!     handle 0.
!
      INTEGER               HANDLS( MAXFIL )
      SAVE                  HANDLS
!
!     The source of each file specified will be stored in the integer
!     array SRCFIL.  If the file is loaded directly, its source
!     will be zero.  If it is loaded as the result of meta-information
!     in a text kernel, the index of the source file in FILES will
!     be stored in SRCES.
!
      INTEGER               SRCES ( MAXFIL )
      SAVE                  SRCES
!
!     The file type of every loaded kernel will be stored in the array
!     TYPES.
!
      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN = 8 )

      CHARACTER*(TYPLEN)    TYPES ( MAXFIL )
      SAVE                  TYPES

!
!     The number of files loaded through this interfaces is kept in the
!     integer LOADED.
!
      INTEGER               LOADED
      SAVE                  LOADED

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )


      INTEGER               NKNOWN
      PARAMETER           ( NKNOWN = 3 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MSGSIZ
      PARAMETER           ( MSGSIZ = 500 )

      CHARACTER*(1)         NORC
      CHARACTER*(FILSIZ)    FIL2LD

      CHARACTER*(FILSIZ)    PVALUE
      CHARACTER*(LNSIZE)    SYMBOL

      CHARACTER*(MSGSIZ)    NOFILE


      CHARACTER*(WDSIZE)    KNOWN ( NKNOWN )
      SAVE                  KNOWN

      CHARACTER*(TYPLEN)    THSTYP

      INTEGER               B
      INTEGER               CURSRC
      INTEGER               D
      INTEGER               DOLLAR
      INTEGER               E
      INTEGER               FIDX
      INTEGER               FILNUM
      INTEGER               FNMLEN
      INTEGER               HITS
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               LIDX
      INTEGER               MYHAND
      INTEGER               N
      INTEGER               N1
      INTEGER               N2
      INTEGER               N3
      INTEGER               NMULT
      INTEGER               NPATHS
      INTEGER               NPVALS
      INTEGER               R
      INTEGER               SIZE
      INTEGER               SRC
      INTEGER               START
      INTEGER               USE

      LOGICAL               ADD
      LOGICAL               DIDCK
      LOGICAL               DIDDSK
      LOGICAL               DIDEK
      LOGICAL               DIDPCK
      LOGICAL               DIDSPK
      LOGICAL               DIDTXT
      LOGICAL               DOCK
      LOGICAL               DODSK
      LOGICAL               DOEK
      LOGICAL               DOMETA
      LOGICAL               DOPCK
      LOGICAL               DOSPK
      LOGICAL               DOTEXT
      LOGICAL               FIRST
      SAVE                  FIRST

      LOGICAL               FND
      LOGICAL               GOTIT
      LOGICAL               OK
      LOGICAL               PATHS
      LOGICAL               SINGLE
      LOGICAL               UPDATE


      DATA                  FIRST     / .TRUE. /
      DATA                  LOADED    /  0  /



      CALL CHKIN  ( 'KEEPER' )
      CALL SETMSG ( 'The routine KEEPER is an umbrella for a ' &
      //            'collection of entry points that manage the ' &
      //            'loading, tracking and unloading of SPICE ' &
      //            'kernels.  KEEPER should not be called ' &
      //            'directly. It is likely that a programming ' &
      //            'error has been made. ' )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)'  )
      CALL CHKOUT ( 'KEEPER' )
      RETURN





!$Procedure FURNSH ( Furnish a program with SPICE kernels )

      ENTRY FURNSH ( FILE )

! $ Abstract
!
!     Load one or more SPICE kernels into a program.
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
!     KERNEL
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         FILE
!
!     INTEGER               FILSIZ
!     PARAMETER           ( FILSIZ = 255 )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FILE       I   SPICE kernel file (text or binary).
!     FILSIZ     P   Maximum file name length.
!
! $ Detailed_Input
!
!     FILE     is a SPICE kernel file. The file may be either binary
!              or text. If the file is a binary SPICE kernel it will
!              be loaded into the appropriate SPICE subsystem. If
!              FILE is a SPICE text kernel it will be loaded into the
!              kernel pool. If FILE is a SPICE meta-kernel containing
!              initialization instructions (through use of the
!              correct kernel pool variables), the files specified in
!              those variables will be loaded into the appropriate
!              SPICE subsystem.
!
!              The SPICE text kernel format supports association of
!              names and data values using a "keyword = value"
!              format. The keyword-value pairs thus defined are
!              called "kernel variables."
!
!              While any information can be placed in a text kernel
!              file, the following string valued kernel variables are
!              recognized by SPICE as meta-kernel keywords:
!
!                   KERNELS_TO_LOAD
!                   PATH_SYMBOLS
!                   PATH_VALUES
!
!              Each kernel variable is discussed below.
!
!              KERNELS_TO_LOAD   is a list of SPICE kernels to be
!                                loaded into a program. If file
!                                names do not fit within the kernel
!                                pool 80 character limit, they may be
!                                continued to subsequent array
!                                elements by placing the continuation
!                                character ('+') at the end of an
!                                element and then placing the
!                                remainder of the file name in the
!                                next array element. (See the
!                                examples below for an illustration
!                                of this technique or consult the
!                                routine STPOOL for further details.)
!
!                                You may use one or more PATH_SYMBOL
!                                assignments (see below) to specify
!                                strings to be substituted for some
!                                part of a file name.
!
!              PATH_SYMBOLS      is a list of strings (without
!                                embedded blanks) which if
!                                encountered following the '$'
!                                character will be replaced with the
!                                corresponding PATH_VALUES string.
!                                Note that PATH_SYMBOLS are
!                                interpreted only in values
!                                associated with the KERNELS_TO_LOAD
!                                variable. There must be a one-to-one
!                                correspondence between the values
!                                supplied for PATH_SYMBOLS and
!                                PATH_VALUES. For the purpose of
!                                determining this correspondence, any
!                                path value that is continued over
!                                multiple array elements counts as a
!                                single value.
!
!              PATH_VALUES       is a list of expansions to use when
!                                PATH_SYMBOLS are encountered. If
!                                path values do not fit within the
!                                kernel pool 80 character limit, they
!                                may be continued in the same way as
!                                file names (see the KERNELS_TO_LOAD
!                                description above).
!
!               These kernel pool variables persist within the kernel
!               pool only until all kernels associated with the
!               variable KERNELS_TO_LOAD have been loaded. Once all
!               specified kernels have been loaded, the variables
!               KERNELS_TO_LOAD, PATH_SYMBOLS and PATH_VALUES are
!               removed from the kernel pool.
!
! $ Detailed_Output
!
!     None. The routine loads various SPICE kernels for use by your
!     application.
!
! $ Parameters
!
!     FILSIZ   is the maximum file name length that can be accommodated
!              by the kernel pool.
!
!     MAXFIL   is the number of entries that can be stored in KEEPER's
!              kernel database. In this version of the toolkit MAXFIL
!              is set to 5300. Each time a kernel is loaded via
!              FURNSH, a database entry is created for that kernel.
!              If a meta-kernel is loaded, a database entry is created
!              for the meta-kernel itself and for all files referenced
!              in the meta-kernel's KERNELS_TO_LOAD specification.
!              Unloading a kernel or meta-kernel deletes database
!              entries created when the file was loaded.
!
!              The parameter MAXFIL is an upper bound on number of
!              SPICE kernels that can be loaded at any time via the
!              KEEPER interface, but the number of kernels that can be
!              loaded may be smaller, since re-loading a loaded kernel
!              or meta-kernel results in creation of additional
!              database entries.
!
!              Kernels loaded into the KEEPER system are subject to
!              constraints imposed by lower-level subsystems. The
!              binary kernel systems (SPK, CK, binary PCK, and EK)
!              have their own limits on the maximum number of kernels
!              that may be loaded.
!
!              The total number of DAF-based files (this set includes
!              SPKs, CKs, and binary PCKs) and DAS-based files (this
!              set includes EKs and DSKs) that may be loaded at any
!              time may not exceed 5000. This limit applies whether
!              the files are loaded via FURNSH or lower-level loaders
!              such as SPKLEF or DAFOPR. File access performance
!              normally will degrade as the number of loaded kernels
!              increases.
!
! $ Exceptions
!
!     1)  If a problem is encountered while trying to load FILE, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     2)  If the input FILE is a meta-kernel and some file in the
!         KERNELS_TO_LOAD assignment cannot be found, or if an error
!         occurs while trying to load a file specified by this
!         assignment, the error is signaled by a routine in the call
!         tree of this routine, and this routine will return. Any files
!         loaded prior to encountering the failure, including those
!         referenced by the KERNELS_TO_LOAD assignment, will remain
!         loaded.
!
!     3)  If an attempt to load a text kernel fails while the kernel is
!         being parsed, any kernel variable assignments made before
!         the failure occurred will be retained in the kernel pool.
!
!     4)  If a PATH_SYMBOLS assignment is specified without a
!         corresponding PATH_VALUES assignment, the error
!         SPICE(NOPATHVALUE) is signaled.
!
!     5)  If a meta-text kernel is supplied to FURNSH that contains
!         instructions specifying that another meta-text kernel be
!         loaded, the error SPICE(RECURSIVELOADING) is signaled.
!
!     6)  If the input file name has non-blank length exceeding FILSIZ
!         characters, the error SPICE(FILENAMETOOLONG) is signaled.
!
!     7)  If the input file is a meta-kernel and some file in the
!         KERNELS_TO_LOAD assignment has name length exceeding FILSIZ
!         characters, the error SPICE(FILENAMETOOLONG) is signaled.
!
!     8)  If the input file is a meta-kernel and some value in the
!         PATH_VALUES assignment has length exceeding FILSIZ
!         characters, the error SPICE(PATHTOOLONG) is signaled.
!
!     9)  If the input file is a meta-kernel and some file in the
!         KERNELS_TO_LOAD assignment has, after symbol substitution,
!         combined name and path length exceeding FILSIZ characters,
!         the error SPICE(FILENAMETOOLONG) is signaled.
!
!     10) If a kernel pool variable name length exceeds its maximum
!         allowed length (see Kernel Required Reading, kernel.req), an
!         error is signaled by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     The input FILE is examined and loaded into the appropriate SPICE
!     subsystem. If the file is a meta-kernel, any kernels specified
!     by the KERNELS_TO_LOAD keyword (and if present, the PATH_SYMBOLS
!     and PATH_VALUES keywords) are loaded as well.
!
! $ Particulars
!
!     This routine provides a uniform interface to the SPICE kernel
!     loading systems. It allows you to easily assemble a list of
!     SPICE kernels required by your application and to modify that set
!     without modifying the source code of programs that make use of
!     these kernels.
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Load the leapseconds kernel naif0007.tls and the planetary
!        ephemeris SPK file de405s.bsp.
!
!           CALL FURNSH ( 'naif0007.tls' )
!           CALL FURNSH ( 'de405s.bsp'   )
!
!
!     2) This example illustrates how you could create a meta-kernel
!        file for a program that requires several text and binary
!        kernels.
!
!        First create a list of the kernels you need in a text file as
!        shown below.
!
!
!           KPL/MK
!
!           File name: furnsh_ex2.tm
!
!           Here are the SPICE kernels required for my application
!           program.
!
!           Note that kernels are loaded in the order listed. Thus
!           we need to list the highest priority kernel last.
!
!
!           \begindata
!
!           KERNELS_TO_LOAD = (
!
!              '/home/mydir/kernels/spk/lowest_priority.bsp',
!              '/home/mydir/kernels/spk/next_priority.bsp',
!              '/home/mydir/kernels/spk/highest_priority.bsp',
!              '/home/mydir/kernels/text/leapsecond.ker',
!              '/home/mydir/kernels+',
!              '/custom+',
!              '/kernel_data/constants.ker',
!              '/home/mydir/kernels/text/sclk.tsc',
!              '/home/mydir/kernels/ck/c-kernel.bc' )
!
!           \begintext
!
!           End of meta-kernel
!
!
!        Note that the file name
!
!           /home/mydir/kernels/custom/kernel_data/constants.ker
!
!        is continued across several lines in the right hand side of
!        the assignment of the kernel variable KERNELS_TO_LOAD.
!
!        Once you've created your list of kernels, call FURNSH near the
!        beginning of your application program to load the meta-kernel
!        automatically at program start up.
!
!           CALL FURNSH ( 'furnsh_ex2.tm' )
!
!        This will cause each of the kernels listed in your meta-kernel
!        to be loaded.
!
!
!     3) This example illustrates how you can simplify the previous
!        kernel list by using PATH_SYMBOLS.
!
!
!           KPL/MK
!
!           File name: furnsh_ex3.tm
!
!           Here are the SPICE kernels required for my application
!           program.
!
!
!           We are going to let A substitute for the directory that
!           contains SPK files; B substitute for the directory that
!           contains C-kernels; and C substitute for the directory that
!           contains text kernels. And we'll let D substitute for
!           a "custom" directory that contains a special planetary
!           constants kernel made just for our mission.
!
!           Note that our PATH_VALUES and the corresponding
!           PATH_SYMBOLS must be listed in the same order.
!
!
!           \begindata
!
!           PATH_VALUES  = ( '/home/mydir/kernels/spk',
!                            '/home/mydir/kernels/ck',
!                            '/home/mydir/kernels/text',
!                            '/home/mydir/kernels/custom/kernel_data' )
!
!           PATH_SYMBOLS = ( 'A',
!                            'B',
!                            'C',
!                            'D'  )
!
!           KERNELS_TO_LOAD = (  '$A/lowest_priority.bsp',
!                                '$A/next_priority.bsp',
!                                '$A/highest_priority.bsp',
!                                '$C/leapsecond.ker',
!                                '$D/constants.ker',
!                                '$C/sclk.tsc',
!                                '$B/c-kernel.bc'         )
!
!           \begintext
!
!           End of meta-kernel
!
!
!     4) This example illustrates continuation of path values. The
!        meta-kernel shown here is a modified version of that from
!        example 3.
!
!
!           KPL/MK
!
!           File name: furnsh_ex4.tm
!
!           Here are the SPICE kernels required for my application
!           program.
!
!           We are going to let A substitute for the directory that
!           contains SPK files; B substitute for the directory that
!           contains C-kernels; and C substitute for the directory that
!           contains text kernels. And we'll let D substitute for
!           a "custom" directory that contains a special planetary
!           constants kernel made just for our mission.
!
!           Note that our PATH_VALUES and the corresponding
!           PATH_SYMBOLS must be listed in the same order.
!
!           The values for path symbols A and D are continued over
!           multiple lines.
!
!           \begindata
!
!           PATH_VALUES  = ( '/very_long_top_level_path_name/mydir/+',
!                            'kernels/spk',
!                            '/home/mydir/kernels/ck',
!                            '/home/mydir/kernels/text',
!                            '/very_long_top_level_path_name+',
!                            '/mydir/kernels/custom+',
!                            '/kernel_data'                )
!
!           PATH_SYMBOLS = ( 'A',
!                            'B',
!                            'C',
!                            'D'  )
!
!           KERNELS_TO_LOAD = (  '$A/lowest_priority.bsp',
!                                '$A/next_priority.bsp',
!                                '$A/highest_priority.bsp',
!                                '$C/leapsecond.ker',
!                                '$D/constants.ker',
!                                '$C/sclk.tsc',
!                                '$B/c-kernel.bc'         )
!
!           \begintext
!
!           End of meta-kernel
!
!
!     5) Load a meta-kernel containing three kernels, and separately,
!        a text kernel and a binary PCK. Count the number of loaded
!        files before and after calling KCLEAR.
!
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: furnsh_ex5.tm
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
!              File name                     Contents
!              ---------                     --------
!              de421.bsp                     Planetary ephemeris
!              pck00009.tpc                  Planet orientation and
!                                            radii
!              naif0012.tls                  Leapseconds
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'de421.bsp',
!                                  'naif0012.tls',
!                                  'pck00009.tpc' )
!
!           \begintext
!
!           End of meta-kernel
!
!
!        Use the PCK kernel below as the binary PCK required for the
!        example.
!
!           earth_latest_high_prec.bpc
!
!
!        Use the FK kernel below as the text kernel required for the
!        example.
!
!           RSSD0002.TF
!
!
!        Example code begins here.
!
!
!              PROGRAM FURNSH_EX5
!              IMPLICIT NONE
!
!        C
!        C     Local variables.
!        C
!              INTEGER               COUNT
!
!        C
!        C     Load several kernel files.
!        C
!              CALL FURNSH ( 'furnsh_ex5.tm'              )
!              CALL FURNSH ( 'RSSD0002.TF'                )
!              CALL FURNSH ( 'earth_latest_high_prec.bpc' )
!
!        C
!        C     Count the number of loaded kernel files.
!        C
!              CALL KTOTAL ( 'ALL', COUNT )
!
!              WRITE(*,'(A,I2)') 'The total number of kernels after '
!             . //               'final FURNSH: ', COUNT
!
!        C
!        C     Clear the KEEPER system, retrieve the number of loaded
!        C     after the clear.
!        C
!              CALL KCLEAR()
!
!              CALL KTOTAL ( 'ALL', COUNT )
!
!              WRITE(*,'(A,I2)') 'The total number of kernels after '
!             . //               'KCLEAR      : ', COUNT
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        The total number of kernels after final FURNSH:  6
!        The total number of kernels after KCLEAR      :  0
!
!
! $ Restrictions
!
!     1)  A meta-kernel cannot reference another meta-kernel.
!
!     2)  Failure during an attempt to load a text kernel or a
!         meta-kernel can result in a subset of the intended kernel
!         variables being set or a subset of the intended files
!         being loaded. FURNSH does not "clean up" so as to undo the
!         effects of a failed load operation.
!
!     3)  When a kernel is specified with a relative path, this path
!         should be valid at the time when FURNSH is called and stay
!         valid for the rest of the application run. This is required
!         because SPICE stores kernel names as provided by the caller
!         and uses them to open and close binary kernels as needed
!         by the DAF/DAS handle manager subsystem (behind the scenes,
!         to allow reading many more binary kernels than available
!         logical units), and to automatically reload into the POOL
!         the rest of text kernels that should stay loaded when a
!         particular text kernel is unloaded.
!
!         Changing the working directory from within an application
!         during an application run after calling FURNSH to load
!         kernels specified using relative paths is likely to
!         invalidate stored paths and prevent open/close and unload
!         operations mentioned above. A simple workaround when this
!         is needed is to specify kernels using absolute paths.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     C.H. Acton         (JPL)
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 5.0.1, 08-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!        Created complete code example from existing code fragments.
!
!        Added KERNEL to $Required_Reading section.
!
!        Added FILSIZ to the $Declarations section.
!
!        Added a restriction about specifying kernels using relative
!        paths to the $Restrictions section.
!
!-    SPICELIB Version 5.0.0, 01-FEB-2017 (NJB) (BVS)
!
!        Updated to support use of DSKs.
!
!        Updated description of MAXFIL in the header.
!
!        Updated the $Author_and_Institution section.
!
!-    SPICELIB Version 4.1.0, 01-JUL-2014 (NJB) (BVS)
!
!        Updated discussion of partially completed kernel loading.
!
!     Last update was 12-APR-2012 (BVS)
!
!        Changed to use SEPOOL instead of STPOOL to reduce loading time
!        for large meta-kernels due to n^2 delay in STPOOL.
!
!-    SPICELIB Version 4.0.1, 10-FEB-2010 (EDW)
!
!        Added mention of the restriction on kernel pool variable
!        names to MAXLEN (defined in pool.f) characters or less.
!
!-    SPICELIB Version 4.0.0, 02-APR-2009 (NJB)
!
!        Continued path values are now supported. FURNSH now rejects
!        file names longer than FILSIZ characters.
!
!-    SPICELIB Version 2.0.3, 27-APR-2007 (NJB)
!
!        Fixed header typo: added quotes to literal string
!        input arguments in example FURNSH calls.
!
!-    SPICELIB Version 2.0.2, 15-NOV-2006 (NJB)
!
!        Added description of parameter MAXFIL to header.
!
!-    SPICELIB Version 2.0.1, 29-JUL-2003 (NJB) (CHA)
!
!        Numerous updates to improve clarity. Some corrections were
!        made.
!
!-    SPICELIB Version 2.0.0, 23-AUG-2001 (WLT)
!
!        Added a call to CVPOOL in FURNSH so that watches that are
!        triggered are triggered by loading Meta-kernels and not by
!        some external interaction with the kernel pool.
!
!-    SPICELIB Version 1.1.0, 19-SEP-2000 (WLT)
!
!        Corrected the error message template used
!        by ZZLDKER
!
!-    SPICELIB Version 1.0.1, 16-DEC-1999 (NJB)
!
!        Documentation fix: corrected second code example in the
!        header of this entry point. The example previously used the
!        kernel variable PATH_NAMES; that name has been replaced with
!        the correct name PATH_VALUES.
!
!-    SPICELIB Version 1.0.0, 01-JUL-1999 (WLT)
!
!-&


! $ Index_Entries
!
!     Load SPICE kernels from a list of kernels
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'FURNSH')

      IF ( FIRST ) THEN

         FIRST    = .FALSE.
         KNOWN(1) = 'KERNELS_TO_LOAD'
         KNOWN(2) = 'PATH_SYMBOLS'
         KNOWN(3) = 'PATH_VALUES'
         LOADED   =  0

         CALL SWPOOL ( 'FURNSH', NKNOWN, KNOWN )
         CALL CVPOOL ( 'FURNSH', UPDATE )

      END IF

!
!     Reject excessively long file names.
!
      IF ( RTRIM(FILE) .GT. FILSIZ ) THEN

         CALL SETMSG ( 'Input file name <#> has length @ characters. ' &
         //            'The limit on the length of file names stored ' &
         //            'by FURNSH is @ characters.'                   )
         CALL ERRCH  ( '#', FILE                                      )
         CALL ERRINT ( '@', RTRIM(FILE)                               )
         CALL ERRINT ( '@', FILSIZ                                    )
         CALL SIGERR ( 'SPICE(FILENAMETOOLONG)'                       )
         CALL CHKOUT ( 'FURNSH'                                       )
         RETURN

      END IF

!
!     Make sure we have room to load at least one more file.
!
      IF ( LOADED .EQ. MAXFIL ) THEN

         CALL SETMSG ( 'There is no room left in KEEPER to load ' &
         //            'another SPICE kernel.  The current limit ' &
         //            'on the number of files that can be ' &
         //            'loaded is #.  If you really need more ' &
         //            'than this many files, you should ' &
         //            'increase the parameter MAXFIL in the ' &
         //            'subroutine KEEPER. ' )

         CALL ERRINT ( '#', MAXFIL )
         CALL SIGERR ( 'SPICE(NOMOREROOM)'  )
         CALL CHKOUT ( 'FURNSH' )
         RETURN

      END IF
!
!     We don't want external interactions with the kernel pool to
!     have any affect on FURNSH's watch so we check the watcher
!     here prior to the call to ZZLDKER.
!
      CALL CVPOOL ( 'FURNSH', UPDATE )

!
!     Set a preliminary value for the error message in case the
!     call to ZZLDKER doesn't succeed.
!
      NOFILE = 'The attempt to load "#" by the routine FURNSH ' &
      //       'failed. It #'

      CALL ZZLDKER ( FILE, NOFILE, THSTYP, MYHAND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'FURNSH' )
         RETURN
      END IF


      LOADED = LOADED + 1
      CURSRC = LOADED

      FILES (LOADED) = FILE
      TYPES (LOADED) = THSTYP
      HANDLS(LOADED) = MYHAND
      SRCES (LOADED) = 0

       CALL CVPOOL ( 'FURNSH', UPDATE )

      IF ( .NOT. UPDATE ) THEN
!
!        Nothing to do.  None of the control variables
!        were set in FILE.
!
         CALL CHKOUT ( 'FURNSH' )
         RETURN

      END IF

!
!     See what is present in the kernel pool: Are any path symbols
!     defined?
!
      CALL DTPOOL ( 'PATH_SYMBOLS', PATHS, NPATHS, NORC )

      IF (  PATHS .AND. ( NORC .EQ. 'C' )  ) THEN
!
!        Make sure that the values are equal in number. We need to
!        use STPOOL to count the path values, since some of them
!        might span multiple array elements.
!
         I = 1
         CALL STPOOL( 'PATH_VALUES', I, '+', PVALUE, SIZE, OK )

         DO WHILE (  OK  .AND.  ( .NOT. FAILED() )  )
!
!           Reject excessively long path names.
!
            IF ( SIZE .GT. FILSIZ ) THEN

               CALL SETMSG ( 'In meta-kernel <#>, the path at ' &
               //            'index # in the PATH_VALUES list has ' &
               //            'length # characters; the limit is # ' &
               //            'characters.'                          )
               CALL ERRCH  ( '#', FILE                              )
               CALL ERRINT ( '#', I                                 )
               CALL ERRINT ( '#', SIZE                              )
               CALL ERRINT ( '#', FILSIZ                            )
               CALL SIGERR ( 'SPICE(PATHTOOLONG)'                   )
               CALL CHKOUT ( 'FURNSH'                               )
               RETURN

            END IF

            I = I + 1
            CALL STPOOL( 'PATH_VALUES', I, '+', PVALUE, SIZE, OK )

         END DO

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'FURNSH' )
            RETURN
         END IF

         NPVALS = I - 1

         IF ( NPVALS .NE. NPATHS ) THEN

            CALL SETMSG ( 'Number of path symbols is #; number of ' &
            //            'path values is #; counts must match.'  )
            CALL ERRINT ( '#', NPATHS                             )
            CALL ERRINT ( '#', NPVALS                             )
            CALL SIGERR ( 'SPICE(PATHMISMATCH)'                   )
            CALL CHKOUT ( 'FURNSH'                                )
            RETURN

         END IF

      ELSE

         PATHS = .FALSE.

      END IF

!
!     This kernel appears to be a legitimate meta-text kernel. Mark
!     it as such and then process its contents.
!
      TYPES(LOADED) = 'META'

!
!     Now load all kernels specified in the KERNELS_TO_LOAD variable.
!
      FILNUM = 1
      FIDX   = 1

      CALL SEPOOL( 'KERNELS_TO_LOAD',  FIDX,    '+', &
                   FIL2LD,             FNMLEN,  LIDX,  OK )

      DO WHILE (  OK  .AND.  ( .NOT. FAILED() )  )
!
!        Reject excessively long file names.
!
         IF ( FNMLEN .GT. FILSIZ ) THEN

            CALL SETMSG ( 'In meta-kernel <#>, the file name at ' &
            //            'index # in the KERNELS_TO_LOAD list has ' &
            //            'length # characters; the limit is # ' &
            //            'characters.'                             )
            CALL ERRCH  ( '#', FILE                                 )
            CALL ERRINT ( '#', FILNUM                               )
            CALL ERRINT ( '#', FNMLEN                               )
            CALL ERRINT ( '#', FILSIZ                               )
            CALL SIGERR ( 'SPICE(FILENAMETOOLONG)'                  )
            CALL CHKOUT ( 'FURNSH'                                  )
            RETURN

         END IF

!
!        Make sure we have room to load at least one more file.
!
         IF ( LOADED .EQ. MAXFIL ) THEN

            CALL SETMSG ( 'There is no room left in KEEPER to ' &
            //            'load another SPICE kernel. The ' &
            //            'current limit on the number of files ' &
            //            'that can be loaded is #.'             )

            CALL ERRINT ( '#', MAXFIL )
            CALL SIGERR ( 'SPICE(NOMOREROOM)'  )
            CALL CHKOUT ( 'FURNSH' )
            RETURN

         END IF

!
!        Resolve any path symbols that may be present.
!        Make sure we have room to load at least one more file.
!
         IF ( PATHS ) THEN

            START  = 1
            DOLLAR = POS( FIL2LD, '$', START )

            DO WHILE ( DOLLAR .GT. 0 )
!
!              Determine the longest path symbol that fits into the
!              current file name.  We fetch path symbols one at a
!              time and see if they match the portion of the
!              string that follows the '$'.  The longest match
!              is the one we use as a symbol.
!
               SIZE = 0
               USE  = 0
               D    = DOLLAR


               DO I = 1, NPATHS

                  CALL GCPOOL( 'PATH_SYMBOLS', I, 1, N, SYMBOL, FND )

                  R =  RTRIM ( SYMBOL )

                  IF (       R  .GT. SIZE &
                       .AND. SAMSUB( SYMBOL,1,R, FIL2LD,D+1,D+R ) ) THEN
                     USE  = I
                     SIZE = R
                  END IF

               END DO
!
!              If we found a matching path symbol, get the corresponding
!              value and put it into the file name.
!
               IF ( USE .GT. 0 ) THEN
!
!                 Get the path value having index USE in the set of
!                 path values. Note that we've already checked that
!                 the path value will fit in PVALUE.
!
                  CALL STPOOL( 'PATH_VALUES', USE, '+', PVALUE, N, FND )

!
!                 When the path is substituted for the symbol, the
!                 total length of the path and file name must fit in
!                 the name buffer.
!
                  IF ( ( FNMLEN + N - SIZE - 1 )  .GT.  FILSIZ ) THEN

                     CALL SETMSG ( 'In meta-kernel <#>, the path at ' &
                     //            'index # in the PATH_SYMBOLS list ' &
                     //            'has # characters and the ' &
                     //            'file name at index # has ' &
                     //            '# characters. The combined path ' &
                     //            'and file name has # characters; ' &
                     //            'the limit is # characters.'       )
                     CALL ERRCH  ( '#', FILE                          )
                     CALL ERRINT ( '#', USE                           )
                     CALL ERRINT ( '#', N                             )
                     CALL ERRINT ( '#', FILNUM                        )
                     CALL ERRINT ( '#', FNMLEN                        )
                     CALL ERRINT ( '#', FNMLEN + N                    )
                     CALL ERRINT ( '#', FILSIZ                        )
                     CALL SIGERR ( 'SPICE(FILENAMETOOLONG)'           )
                     CALL CHKOUT ( 'FURNSH'                           )
                     RETURN

                  END IF

                  CALL REPSUB( FIL2LD, D, D+SIZE, PVALUE(1:N), FIL2LD )

               END IF
!
!              Look for the next occurrence of a '$' after the last
!              place we found one.
!
               START  = DOLLAR + 1
               DOLLAR = POS( FIL2LD, '$', START )

            END DO

         END IF
!
!        If any path symbols were present, they have now been
!        resolved.  Let ZZLDKER handle the task of loading this
!        kernel.  Make up a message template for use if ZZLDKER
!        runs into a problem.
!
         NOFILE = 'The @ file ''#'' specified by KERNELS_TO_LOAD ' &
         //       'in the file @ #'

         CALL REPMOT ( NOFILE, '@', FILNUM, 'L', NOFILE )
         CALL REPMC  ( NOFILE, '@', FILE,        NOFILE )

         CALL ZZLDKER( FIL2LD, NOFILE, THSTYP, MYHAND )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'FURNSH' )
            RETURN
         END IF

         IF ( THSTYP .EQ. 'TEXT' ) THEN
!
!           See if we stepped on any of the recognized variables.  If
!           we did, there's no point in trying to continue.
!
            CALL CVPOOL ( 'FURNSH', UPDATE )

            IF ( UPDATE ) THEN
!
!              First clean up the debris created by this attempt
!              at recursion.
!
               DO I = 1, NKNOWN
                  CALL DVPOOL ( KNOWN(I) )
               END DO
!
!              Take care of any watcher activation caused by the
!              mop-up of the preceding loop.
!
               CALL CVPOOL ( 'FURNSH', UPDATE )

               CALL SETMSG ( 'Hmmm.  This is interesting. In the ' &
               //            'meta-text kernel ''#'' you''ve ' &
               //            'requested that the text kernel ' &
               //            '''#'' be loaded. This second file ' &
               //            'is also a "meta-text" kernel and ' &
               //            'specifies new kernel loading ' &
               //            'instructions. Although you receive ' &
               //            'high marks for creativity, this ' &
               //            'path is fraught with peril and can ' &
               //            'not be supported by FURNSH. ' )

               CALL ERRCH  ( '#', FILE   )
               CALL ERRCH  ( '#', FIL2LD )
               CALL SIGERR ( 'SPICE(RECURSIVELOADING)' )
               CALL CHKOUT ( 'FURNSH' )
               RETURN
            END IF

         END IF
!
!        Add the latest file loaded to our database of loaded
!        files.
!
         LOADED         = LOADED + 1
         FILES (LOADED) = FIL2LD
         TYPES (LOADED) = THSTYP
         HANDLS(LOADED) = MYHAND
         SRCES (LOADED) = CURSRC

!
!        Get the name of the next file to load.
!
         FILNUM = FILNUM + 1
         FIDX   = LIDX   + 1

         CALL SEPOOL( 'KERNELS_TO_LOAD',  FIDX,    '+', &
                       FIL2LD,            FNMLEN,  LIDX,  OK )

      END DO

!
!     Last Step.  Remove the special variables from the kernel pool.
!
      DO I = 1, NKNOWN
         CALL DVPOOL ( KNOWN(I) )
      END DO

      CALL CVPOOL ( 'FURNSH', UPDATE )

      CALL CHKOUT ( 'FURNSH' )
      RETURN



!$Procedure KTOTAL ( Kernel Totals )

      ENTRY KTOTAL ( KIND, COUNT )

! $ Abstract
!
!     Return the number of kernels of a specified type that are
!     currently loaded via the FURNSH interface.
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
!     KERNEL
!
! $ Keywords
!
!     KERNEL
!
! $ Declarations
!
!     CHARACTER*(*)         KIND
!     INTEGER               COUNT
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     KIND       I   A list of kinds of kernels to count.
!     COUNT      O   The number of kernels of type KIND.
!
! $ Detailed_Input
!
!     KIND     is a list of types of kernels to count when computing
!              loaded kernels. KIND should consist of a list of words of
!              kernels to examine. Recognized types are
!
!                 SPK  --- All SPK files are counted in the total.
!                 CK   --- All CK files are counted in the total.
!                 PCK  --- All binary PCK files are counted in the
!                          total.
!                 DSK  --- All DSK files are counted in the total.
!                 EK   --- All EK files are counted in the total.
!                 TEXT --- All text kernels that are not meta-text
!                          kernels are included in the total.
!                 META --- All meta-text kernels are counted in the
!                          total.
!                 ALL  --- Every type of kernel is counted in the
!                          total.
!
!              KIND is case insensitive. If a word appears in KIND
!              that is not one of those listed above, it is ignored.
!
!              When KIND consists of multiple words, the words must
!              be separated by blanks. Examples of valid lists are the
!              strings
!
!                 'SPK CK TEXT'
!                 'SPK CK text'
!                 'PCK DSK'
!                 'CK'
!                 'ALL'
!
!              See the $Examples section for illustrations of the
!              use of KIND.
!
! $ Detailed_Output
!
!     COUNT    is the number of kernels loaded through FURNSH that
!              belong to the list specified by KIND.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If a word on the list specified by KIND is not recognized,
!         it is ignored.
!
!     2)  If KIND is blank, or none of the words in KIND is on the
!         list specified above, COUNT will be returned as zero.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     KTOTAL allows you to easily determine the number of kernels
!     loaded via the interface FURNSH that are of a type of interest.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Load a meta-kernel with a PCK, an LSK and an SPK, and
!        separately, a text kernel and a binary PCK. Show the
!        total number of kernels and meta-kernels loaded. Determine the
!        number of text kernels loaded, and the number of binary
!        kernels.
!
!        Unload all kernels and clear the kernel pool using
!        KCLEAR, and check that no kernels are loaded.
!
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: ktotal_ex1.tm
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
!              File name                     Contents
!              ---------                     --------
!              de421.bsp                     Planetary ephemeris
!              pck00009.tpc                  Planet orientation and
!                                            radii
!              naif0012.tls                  Leapseconds
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'de421.bsp',
!                                  'naif0012.tls',
!                                  'pck00009.tpc' )
!
!           \begintext
!
!           End of meta-kernel
!
!
!        Use the PCK kernel below as the binary PCK required for the
!        example.
!
!           earth_latest_high_prec.bpc
!
!
!        Use the FK kernel below as the text kernel required for the
!        example.
!
!           RSSD0002.TF
!
!
!        Example code begins here.
!
!
!              PROGRAM KTOTAL_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local variables.
!        C
!              INTEGER               COUNT
!
!        C
!        C     Load several kernel files.
!        C
!              CALL FURNSH ( 'ktotal_ex1.tm'              )
!              CALL FURNSH ( 'RSSD0002.TF'                )
!              CALL FURNSH ( 'earth_latest_high_prec.bpc' )
!
!        C
!        C     Count the number of loaded kernel files.
!        C
!              CALL KTOTAL ( 'ALL', COUNT )
!
!              WRITE(*,'(A,I2)') 'The total number of kernels after '
!             . //               'final FURNSH: ', COUNT
!
!        C
!        C     Count the number of meta-kernels.
!        C
!              CALL KTOTAL ( 'META', COUNT )
!
!              WRITE(*,'(A,I2)') 'The total number of meta-kernels  '
!             . //               '            : ', COUNT
!
!        C
!        C     Count the number of text kernels.
!        C
!              CALL KTOTAL ( 'TEXT', COUNT )
!
!              WRITE(*,'(A,I2)') 'The total number of text kernels  '
!             . //               '            : ', COUNT
!
!        C
!        C     Count the number of binary kernels. These kernels
!        C     are of type CK, DSK, EK, PCK or SPK.
!        C
!              CALL KTOTAL ( 'CK DSK EK PCK SPK', COUNT )
!              WRITE(*,'(A,I2)') 'The total number of binary kernels'
!             . //               '            : ', COUNT
!
!        C
!        C     Clear the KEEPER system, retrieve the number of loaded
!        C     after the clear.
!        C
!              CALL KCLEAR()
!
!              CALL KTOTAL ( 'ALL', COUNT )
!
!              WRITE(*,'(A,I2)') 'The total number of kernels after '
!             . //               'KCLEAR      : ', COUNT
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        The total number of kernels after final FURNSH:  6
!        The total number of meta-kernels              :  1
!        The total number of text kernels              :  3
!        The total number of binary kernels            :  2
!        The total number of kernels after KCLEAR      :  0
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 5.0.1, 25-OCT-2021 (JDR) (NJB)
!
!        Edited the header to comply with NAIF standard.
!        Created complete code example from existing code fragments.
!
!        Updated $Detailed_Input description of input argument KIND to
!        illustrate use of multi-word lists. Added KERNEL to the list
!        of required readings.
!
!-    SPICELIB Version 5.0.0, 01-FEB-2017 (NJB) (BVS)
!
!        Updated to support use of DSKs.
!
!        Updated the $Author_and_Institution section.
!
!-    SPICELIB Version 1.1.0, 02-APR-2009 (NJB)
!
!        Deleted reference to unneeded variable DOALL.
!
!-    SPICELIB Version 1.0.0, 01-JUL-1999 (WLT)
!
!-&


! $ Index_Entries
!
!     Number of loaded kernels of a given type
!
!-&


      IF ( LOADED .EQ. 0 ) THEN
         COUNT = 0
         RETURN
      END IF

      CALL CHKIN  ( 'KTOTAL' )
!
!     Parse KIND to see which kernels are of interest.
!
      DOSPK  = .FALSE.
      DOCK   = .FALSE.
      DODSK  = .FALSE.
      DOTEXT = .FALSE.
      DOMETA = .FALSE.
      DOEK   = .FALSE.
      DOPCK  = .FALSE.
      START  = 1

      CALL FNDNWD ( KIND, START, B, E )


      DO WHILE ( B .GT. 0 )

         IF ( EQSTR( KIND(B:E), 'ALL' ) )THEN
            COUNT = LOADED
            CALL CHKOUT ( 'KTOTAL' )
            RETURN
         ELSE
            DOCK   = DOCK   .OR. EQSTR ( KIND(B:E), 'CK' )
            DODSK  = DODSK  .OR. EQSTR ( KIND(B:E), 'DSK' )
            DOEK   = DOEK   .OR. EQSTR ( KIND(B:E), 'EK' )
            DOMETA = DOMETA .OR. EQSTR ( KIND(B:E), 'META' )
            DOPCK  = DOPCK  .OR. EQSTR ( KIND(B:E), 'PCK' )
            DOSPK  = DOSPK  .OR. EQSTR ( KIND(B:E), 'SPK' )
            DOTEXT = DOTEXT .OR. EQSTR ( KIND(B:E), 'TEXT' )
         END IF

         START = E + 1
         CALL FNDNWD ( KIND, START, B, E )

      END DO

      COUNT = 0


      DO I = 1, LOADED

         ADD =      ( TYPES(I) .EQ. 'CK'   .AND. DOCK   ) &
               .OR. ( TYPES(I) .EQ. 'DSK'  .AND. DODSK  ) &
               .OR. ( TYPES(I) .EQ. 'EK'   .AND. DOEK   ) &
               .OR. ( TYPES(I) .EQ. 'META' .AND. DOMETA ) &
               .OR. ( TYPES(I) .EQ. 'PCK'  .AND. DOPCK  ) &
               .OR. ( TYPES(I) .EQ. 'SPK'  .AND. DOSPK  ) &
               .OR. ( TYPES(I) .EQ. 'TEXT' .AND. DOTEXT )

         IF ( ADD ) THEN
            COUNT = COUNT + 1
         END IF

      END DO

      CALL CHKOUT ( 'KTOTAL' )
      RETURN


!$Procedure KDATA ( Kernel Data )

      ENTRY KDATA ( WHICH, KIND, FILE, FILTYP, SRCFIL, HANDLE, FOUND )

! $ Abstract
!
!     Return data for the nth kernel that is among a list of specified
!     kernel types.
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
!     KERNEL
!
! $ Keywords
!
!     KERNEL
!
! $ Declarations
!
!     INTEGER               WHICH
!     CHARACTER*(*)         KIND
!     CHARACTER*(*)         FILE
!     CHARACTER*(*)         FILTYP
!     CHARACTER*(*)         SRCFIL
!     INTEGER               HANDLE
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     WHICH      I   Index of kernel to fetch from the list of kernels.
!     KIND       I   The kind of kernel to which fetches are limited.
!     FILE       O   The name of the kernel file.
!     FILTYP     O   The type of the kernel.
!     SRCFIL     O   Name of the source file used to load FILE.
!     HANDLE     O   The handle attached to FILE.
!     FOUND      O   .TRUE. if the specified file could be located.
!
! $ Detailed_Input
!
!     WHICH    is the number of the kernel to fetch (matching the type
!              specified by KIND) from the list of kernels that have
!              been loaded through the routine FURNSH but that have not
!              been unloaded through the routine UNLOAD.
!
!              The range of WHICH is 1 to COUNT, where COUNT is the
!              number of kernels loaded via FURNSH of type KIND. This
!              count may be obtained by calling KTOTAL. See the
!              $Examples section for an illustrative example.
!
!     KIND     is a list of types of kernels to be considered when
!              fetching kernels from the list of loaded kernels. KIND
!              should consist of words from list of kernel types
!              given below.
!
!                 SPK  --- All SPK files are counted in the total.
!                 CK   --- All CK files are counted in the total.
!                 DSK  --- All DSK files are counted in the total.
!                 PCK  --- All binary PCK files are counted in the
!                          total.
!                 EK   --- All EK files are counted in the total.
!                 TEXT --- All text kernels that are not meta-text
!                          kernels are included in the total.
!                 META --- All meta-text kernels are counted in the
!                          total.
!                 ALL  --- Every type of kernel is counted in the
!                          total.
!
!              KIND is case insensitive. If a word appears in KIND
!              that is not one of those listed above, it is ignored.
!
!              When KIND consists of multiple words, the words must
!              be separated by blanks. Examples of valid lists are the
!              strings
!
!                 'SPK CK TEXT'
!                 'SPK CK text'
!                 'PCK DSK'
!                 'CK'
!                 'ALL'
!
!              See the routine KTOTAL for examples of the use of KIND.
!
! $ Detailed_Output
!
!     FILE     is the name of the file having index WHICH in the
!              sequence of files of type KIND that is currently loaded
!              via FURNSH. FILE will be blank if there is not such
!              kernel loaded.
!
!     FILTYP   is the type of the kernel specified by FILE. FILE
!              will be blank if there is no file matching the
!              specification of WHICH and KIND.
!
!     SRCFIL   is the name of the source file that was used to
!              specify FILE as one to load. If FILE was loaded
!              directly via a call to FURNSH, SRCFIL will be blank.
!              If there is no file matching the specification of
!              WHICH and KIND, SRCFIL will be blank.
!
!     HANDLE   is the handle attached to FILE if it is a binary
!              kernel. If FILE is a text kernel or meta-text kernel
!              HANDLE will be zero. If there is no file matching
!              the specification of WHICH and KIND, HANDLE will be
!              set to zero.
!
!     FOUND    is returned .TRUE. if a FILE matching the specification
!              of WHICH and KIND exists. If there is no such file,
!              FOUND will be set to .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If a file is not loaded matching the specification of WHICH
!         and KIND, FOUND will be .FALSE., FILE, FILTYP, and SRCFIL will
!         be blank and HANDLE will be set to zero.
!
!     2)  If any of FILE, FILTYP or SRCFIL output strings has length
!         too short to contain the corresponding output string, the
!         string is truncated on the right.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine allows you to determine which kernels have been
!     loaded via FURNSH and to obtain information sufficient to directly
!     query those files.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Load a meta-kernel with a PCK, an LSK and an SPK and loop over
!        the loaded kernels, outputting file information for each of
!        them.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: kdata_ex1.tm
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
!              File name                     Contents
!              ---------                     --------
!              de421.bsp                     Planetary ephemeris
!              pck00009.tpc                  Planet orientation and
!                                            radii
!              naif0009.tls                  Leapseconds
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'de421.bsp',
!                                  'pck00009.tpc',
!                                  'naif0009.tls'  )
!
!           \begintext
!
!           End of meta-kernel
!
!
!        Example code begins here.
!
!
!              PROGRAM KDATA_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local constants.
!        C
!              INTEGER               FNAMLN
!              PARAMETER           ( FNAMLN = 256 )
!
!              INTEGER               FTYPLN
!              PARAMETER           ( FTYPLN = 33 )
!
!              INTEGER               SRCLEN
!              PARAMETER           ( SRCLEN = 256 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(FNAMLN)    FILE
!              CHARACTER*(FTYPLN)    FILTYP
!              CHARACTER*(SRCLEN)    SRCFIL
!
!              INTEGER               COUNT
!              INTEGER               HANDLE
!              INTEGER               WHICH
!
!              LOGICAL               FOUND
!
!        C
!        C     Load several kernel files.
!        C
!              CALL FURNSH ( 'kdata_ex1.tm' )
!
!        C
!        C     Count the number of loaded kernel files.
!        C
!              CALL KTOTAL ( 'ALL', COUNT )
!
!              DO WHICH= 1, COUNT + 1
!
!                 CALL KDATA ( WHICH, 'ALL',   FILE, FILTYP,
!             .                SRCFIL, HANDLE, FOUND        )
!
!                 IF ( FOUND ) THEN
!
!                    WRITE(*,*) 'Index : ', WHICH
!                    WRITE(*,*) 'File  : ', FILE
!                    WRITE(*,*) 'Type  : ', FILTYP
!                    WRITE(*,*) 'Source: ', SRCFIL
!                    WRITE(*,*) 'Handle: ', HANDLE
!                    WRITE(*,*) ' '
!
!                 ELSE
!
!                    WRITE(*,*) 'No kernel found with index: ', WHICH
!
!                 END IF
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
!         Index :            1
!         File  : kdata_ex1.tm
!         Type  : META
!         Source:
!         Handle:            0
!
!         Index :            2
!         File  : de421.bsp
!         Type  : SPK
!         Source: kdata_ex1.tm
!         Handle:            1
!
!         Index :            3
!         File  : pck00009.tpc
!         Type  : TEXT
!         Source: kdata_ex1.tm
!         Handle:            0
!
!         Index :            4
!         File  : naif0009.tls
!         Type  : TEXT
!         Source: kdata_ex1.tm
!         Handle:            0
!
!         No kernel found with index:            5
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 5.1.0, 08-AUG-2021 (JDR) (NJB)
!
!        Changed argument name SOURCE to SRCFIL for consistency with
!        other routines.
!
!        Edited the header to comply with NAIF standard.
!        Created complete code example from existing code fragments.
!
!        Updated $Detailed_Input description of input arguments KIND, to
!        illustrate use of multi-word lists, and WHICH, to describe its
!        range.
!
!        Added entry #2 to $Exceptions section. Added KERNEL to the list
!        of required readings.
!
!-    SPICELIB Version 5.0.0, 01-FEB-2017 (NJB) (BVS)
!
!        Updated to support use of DSKs.
!
!        Updated the $Author_and_Institution section.
!
!-    SPICELIB Version 1.1.0, 02-APR-2009 (NJB)
!
!        Deleted reference to unneeded variable DOALL.
!
!-    SPICELIB Version 1.0.1, 06-DEC-2002 (NJB)
!
!        Typo in header example was corrected.
!
!-    SPICELIB Version 1.0.0, 01-JUL-1999 (WLT)
!
!-&


! $ Index_Entries
!
!     Retrieve information on loaded SPICE kernels
!
!-&


      FILE   = ' '
      FILTYP = ' '
      SRCFIL = ' '
      HANDLE =  0
      FOUND  = .FALSE.

      IF ( WHICH .LT. 1 .OR. WHICH .GT. LOADED ) THEN
         RETURN
      END IF
!
!     Parse KIND to see which kernels are of interest.
!
      DOSPK  = .FALSE.
      DOCK   = .FALSE.
      DODSK  = .FALSE.
      DOTEXT = .FALSE.
      DOMETA = .FALSE.
      DOEK   = .FALSE.
      DOPCK  = .FALSE.
      START  = 1
      CALL FNDNWD ( KIND, START, B, E )

      DO WHILE ( B .GT. 0 )

         IF ( EQSTR( KIND(B:E), 'ALL' ) )THEN
!
!           There's no point in going on, we can fill in the output
!           variables right now.
!
            FOUND  = .TRUE.
            FILE   = FILES (WHICH)
            FILTYP = TYPES (WHICH)
            HANDLE = HANDLS(WHICH)

            IF ( SRCES(WHICH) .NE. 0 ) THEN
               SRCFIL = FILES( SRCES(WHICH) )
            END IF
            RETURN

         ELSE
            DOCK   = DOCK   .OR. EQSTR ( KIND(B:E), 'CK' )
            DODSK  = DODSK  .OR. EQSTR ( KIND(B:E), 'DSK' )
            DOEK   = DOEK   .OR. EQSTR ( KIND(B:E), 'EK' )
            DOMETA = DOMETA .OR. EQSTR ( KIND(B:E), 'META' )
            DOPCK  = DOPCK  .OR. EQSTR ( KIND(B:E), 'PCK' )
            DOSPK  = DOSPK  .OR. EQSTR ( KIND(B:E), 'SPK' )
            DOTEXT = DOTEXT .OR. EQSTR ( KIND(B:E), 'TEXT' )
         END IF

         START = E + 1
         CALL FNDNWD ( KIND, START, B, E )

      END DO

!
!     Examine the loaded kernels one at a time until we match
!     WHICH files of the specified KIND.
!
      HITS = 0

      DO I = 1, LOADED

         ADD =      ( TYPES(I) .EQ. 'CK'   .AND. DOCK   ) &
               .OR. ( TYPES(I) .EQ. 'DSK'  .AND. DODSK  ) &
               .OR. ( TYPES(I) .EQ. 'EK'   .AND. DOEK   ) &
               .OR. ( TYPES(I) .EQ. 'META' .AND. DOMETA ) &
               .OR. ( TYPES(I) .EQ. 'PCK'  .AND. DOPCK  ) &
               .OR. ( TYPES(I) .EQ. 'SPK'  .AND. DOSPK  ) &
               .OR. ( TYPES(I) .EQ. 'TEXT' .AND. DOTEXT )

         IF ( ADD ) THEN

            HITS = HITS + 1
!
!           If we've reached the specified number, fill in the
!           requested information and return.
!
            IF ( HITS .EQ. WHICH ) THEN

               FOUND  = .TRUE.
               FILE   = FILES (I)
               FILTYP = TYPES (I)
               HANDLE = HANDLS(I)

               IF ( SRCES(I) .NE. 0 ) THEN
                  SRCFIL = FILES( SRCES(I) )
               END IF
               RETURN
            END IF
         END IF

      END DO

      RETURN



!$Procedure KINFO ( Kernel Information )

      ENTRY KINFO ( FILE, FILTYP, SRCFIL, HANDLE, FOUND )

! $ Abstract
!
!     Return information about a specific kernel
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
!
! $ Declarations
!
!     CHARACTER*(*)         FILE
!     CHARACTER*(*)         FILTYP
!     CHARACTER*(*)         SRCFIL
!     INTEGER               HANDLE
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FILE       I   Name of a kernel to fetch information for
!     FILTYP     O   The type of the kernel
!     SRCFIL     O   Name of the source file used to load FILE.
!     HANDLE     O   The handle attached to FILE.
!     FOUND      O   .TRUE. if the specified file could be located.
!
! $ Detailed_Input
!
!     FILE     is the name of a kernel file for which KEEPER
!              information is desired.
!
! $ Detailed_Output
!
!     FILTYP   is the type of the kernel specified by FILE.  FILE
!              will be blank if FILE is not on the list of loaded
!              kernels.
!
!     SRCFIL   is the name of the source file that was used to
!              specify FILE as one to load. If FILE was loaded
!              directly via a call to FURNSH, SRCFIL will be blank.
!              If FILE is not on the list of loaded kernels, SRCFIL
!              will be blank
!
!     HANDLE   is the handle attached to FILE if it is a binary
!              kernel. If FILE is a text kernel or meta-text kernel
!              HANDLE will be zero. If FILE is not on the list of
!              loaded kernels, HANDLE will be set to zero.
!
!     FOUND    is returned .TRUE. if FILE is on the KEEPER list of
!              loaded kernels. If there is no such file, FOUND will
!              be set to .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the specified file is not on the list of files that are
!         currently loaded via the interface FURNSH, FOUND will be
!         .FALSE., HANDLE will be set to zero and FILTYP and SRCFIL will
!         be set to blanks.
!
!     2)  If any of  FILTYP or SRCFIL output strings has length too
!         short to contain the corresponding output string, the string
!         is truncated on the right.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This entry point allows you to request information directly
!     for a specific SPICE kernel.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Suppose you wish to determine the types of kernels loaded
!        by a given meta-kernel. The following code example shows
!        how you might use this routine to do this.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: kinfo_ex1.tm
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
!              File name                     Contents
!              ---------                     --------
!              de421.bsp                     Planetary ephemeris
!              pck00008.tpc                  Planet orientation and
!                                            radii
!              naif0009.tls                  Leapseconds
!
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'de421.bsp',
!                                  'pck00008.tpc',
!                                  'naif0009.tls'  )
!
!           \begintext
!
!           End of meta-kernel
!
!
!        Example code begins here.
!
!
!              PROGRAM KINFO_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              LOGICAL               EQSTR
!
!        C
!        C     Local constants.
!        C
!              INTEGER               FILLEN
!              PARAMETER           ( FILLEN = 32 )
!
!              INTEGER               TYPLEN
!              PARAMETER           ( TYPLEN = 33 )
!
!              INTEGER               SRCLEN
!              PARAMETER           ( SRCLEN = 256 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(FILLEN)    FILE
!              CHARACTER*(TYPLEN)    FILTYP
!              CHARACTER*(SRCLEN)    SRCFIL
!
!              INTEGER               COUNT
!              INTEGER               HANDLE
!              INTEGER               WHICH
!
!              LOGICAL               FOUND
!
!        C
!        C     Load the meta-kernel.
!        C
!              CALL FURNSH ( 'kinfo_ex1.tm' )
!
!        C
!        C     Find out the total number of kernels in the kernel pool.
!        C
!              CALL KTOTAL ( 'all', COUNT )
!
!              IF ( COUNT .EQ. 0 ) THEN
!
!                 WRITE(*,*) 'No files loaded at this time.'
!
!              ELSE
!
!                 WRITE(*,*) 'The loaded files files are: '
!                 WRITE(*,*) ' '
!
!              END IF
!
!        C
!        C     Find the file name, type and source for each of the
!        C     kernels in the kernel pool and print its type.
!        C
!              DO WHICH= 1, COUNT
!
!                 CALL KDATA ( WHICH,  'all',  FILE, FILTYP,
!             .                SRCFIL, HANDLE, FOUND        )
!
!                 CALL KINFO ( FILE, FILTYP, SRCFIL, HANDLE, FOUND )
!
!                 IF ( EQSTR( FILTYP, "SPK" ) ) THEN
!
!                    WRITE(*,*) FILE, ' is an SPK file.'
!
!                 ELSE IF ( EQSTR( FILTYP, "CK" ) ) THEN
!
!                    WRITE(*,*) FILE, ' is a CK file.'
!
!                 ELSE IF ( EQSTR( FILTYP, "PCK" ) ) THEN
!
!                    WRITE(*,*) FILE, ' is a PCK file.'
!
!                 ELSE IF ( EQSTR( FILTYP, "DSK" ) ) THEN
!
!                    WRITE(*,*) FILE, ' is a DSK file.'
!
!                 ELSE IF ( EQSTR( FILTYP, "EK" ) ) THEN
!
!                    WRITE(*,*) FILE, ' is an EK file.'
!
!                 ELSE IF ( EQSTR( FILTYP, "META" ) ) THEN
!
!                    WRITE(*,*) FILE, ' is a meta-kernel file.'
!
!                 ELSE
!
!                    WRITE(*,*) FILE, ' is a text kernel.'
!
!                 END IF
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
!         The loaded files files are:
!
!         kinfo_ex1.tm                     is a meta-kernel file.
!         de421.bsp                        is an SPK file.
!         pck00008.tpc                     is a text kernel.
!         naif0009.tls                     is a text kernel.
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 5.1.0, 08-AUG-2021 (JDR)
!
!        Changed argument name SOURCE to SRCFIL for consistency with
!        other routines.
!
!        Edited the header to comply with NAIF standard.
!        Created complete code example from existing code fragments.
!
!        Added entry #2 to $Exceptions section.
!
!-    SPICELIB Version 5.0.0, 01-FEB-2017 (NJB) (BVS)
!
!        Updated to support use of DSKs.
!
!        Updated the $Author_and_Institution section.
!
!-    SPICELIB Version 1.0.0, 01-JUL-1999 (WLT)
!
!-&


! $ Index_Entries
!
!     Fetch information about a loaded SPICE kernel
!
!-&


      FILTYP = ' '
      SRCFIL = ' '
      HANDLE =  0
      FOUND  = .FALSE.

      I = ISRCHC ( FILE, LOADED, FILES )

      IF ( I .GT. 0 ) THEN

         FOUND  = .TRUE.
         FILTYP = TYPES (I)
         HANDLE = HANDLS(I)

         IF ( SRCES(I) .NE. 0 ) THEN
            SRCFIL = FILES( SRCES(I) )
         END IF

      END IF

      RETURN




!$Procedure KCLEAR ( Keeper clear )

      ENTRY KCLEAR

! $ Abstract
!
!     Clear the KEEPER subsystem: unload all kernels, clear the kernel
!     pool, and re-initialize the subsystem. Existing watches on kernel
!     variables are retained.
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
!
! $ Declarations
!
!     None.
!
! $ Brief_I/O
!
!     None.
!
! $ Detailed_Input
!
!     None. This routine operates by side effects. See $Particulars
!     below.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs when setting a kernel pool watch or
!         checking watched variables, the error is signaled by a routine
!         in the call tree of this routine.
!
! $ Files
!
!     See $Particulars.
!
! $ Particulars
!
!     This entry point allows you re-initialize the KEEPER system with
!     a single call.
!
!     This routine unloads all kernels from their kernel-type-specific
!     kernel management subsystems (SPKBSR, CKBSR, etc.), clears the
!     kernel pool, clears KEEPER's internal file database, and re-sets
!     the watch status for the kernel variables used to load kernels
!     via meta-kernels. As a side effect of clearing the kernel pool,
!     all watched variables are marked as updated. Note that clearing
!     the kernel pool does not delete watches (aka "watchers"). Watches
!     can be deleted by calling the POOL entry point DWPOOL.
!
!     This capability, though implemented in Fortran, is particularly
!     relevant to SPICE implementations such as Icy, for which the
!     state of the KEEPER system persists after any Icy-based IDL
!     script is run. Successive runs of Icy-based scripts may perform
!     in unexpected ways when scripts access data loaded during runs of
!     previous scripts.
!
!     Cleaning up after such programs using explicit UNLOAD commands is
!     tedious and error-prone. One call to this routine sets the
!     KEEPER system to its initial state, preventing unintentional
!     interaction between scripts via KEEPER's state.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Load a meta-kernel containing three kernels, and count the
!        number of files in the kernel pool before and after calling
!        KCLEAR.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: kclear_ex1.tm
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
!              File name                     Contents
!              ---------                     --------
!              de421.bsp                     Planetary ephemeris
!              pck00009.tpc                  Planet orientation and
!                                            radii
!              naif0012.tls                  Leapseconds
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'de421.bsp',
!                                  'naif0012.tls',
!                                  'pck00009.tpc' )
!
!           \begintext
!
!           End of meta-kernel
!
!
!        Example code begins here.
!
!
!              PROGRAM KCLEAR_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local variables.
!        C
!              INTEGER               COUNT
!
!        C
!        C     Load several kernel files.
!        C
!              CALL FURNSH ( 'kclear_ex1.tm' )
!
!        C
!        C     Count the number of loaded kernel files.
!        C
!              CALL KTOTAL ( 'ALL', COUNT )
!
!              WRITE(*,'(A,I2)') 'The total number of kernels after '
!             . //               'FURNSH: ', COUNT
!
!        C
!        C     Clear the KEEPER system, retrieve the number of loaded
!        C     after the clear.
!        C
!              CALL KCLEAR()
!
!              CALL KTOTAL ( 'ALL', COUNT )
!
!              WRITE(*,'(A,I2)') 'The total number of kernels after '
!             . //               'KCLEAR: ', COUNT
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        The total number of kernels after FURNSH:  4
!        The total number of kernels after KCLEAR:  0
!
!
! $ Restrictions
!
!     1)  Calling this routine will wipe out any kernel pool data
!         inserted via the SPICELIB API routines to put data into the
!         kernel pool (PCPOOL, PDPOOL and PIPOOL).
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
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 5.0.1, 08-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example from existing code fragments.
!
!        Improved $Restrictions section.
!
!-    SPICELIB Version 5.0.0, 01-FEB-2017 (NJB) (BVS)
!
!        Updated to support use of DSKs.
!
!        Updated the $Author_and_Institution section.
!
!-    SPICELIB Version 1.0.1, 01-JUL-2014 (NJB) (EDW)
!
!        Updated the discussion of kernel variable watchers.
!
!     Last update was 13-APR-2011 (EDW)
!
!        Trivial edit to $Restrictions, replaced P*POOL with
!        PXPOOL. The "*" character causes the HTML documentation
!        script to create a link for the "POOL" substring.
!
!-    SPICELIB Version 1.0.0, 15-NOV-2006 (NJB)
!
!-&


! $ Index_Entries
!
!     Re-initialize the keeper system
!     Clear the keeper system
!     Unload all kernels
!
!-&


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'KCLEAR' )

!
!     Unloading all kernels is actually much less work than
!     unloading just a few of them.  We unload all of the
!     binary kernels via the "unload" routines for their
!     respective subsystems, then clear the kernel pool.
!
      DO I = 1, LOADED

         IF (      TYPES(I) .EQ. 'SPK' ) THEN

            CALL SPKUEF ( HANDLS(I) )

         ELSE IF ( TYPES(I) .EQ. 'CK' ) THEN

            CALL CKUPF ( HANDLS(I) )

         ELSE IF ( TYPES(I) .EQ. 'PCK' ) THEN

            CALL PCKUOF ( HANDLS(I) )

         ELSE IF ( TYPES(I) .EQ. 'EK' ) THEN

            CALL EKUEF ( HANDLS(I) )

         ELSE IF ( TYPES(I) .EQ. 'DSK' ) THEN

            CALL ZZDSKUSF ( HANDLS(I) )

         END IF

      END DO

      CALL CLPOOL

!
!     Although it's not strictly necessary, we initialize
!     KEEPER's database arrays.  This step may occasionally
!     be helpful for debugging.
!
      DO I = 1, LOADED

         FILES (I)  = ' '
         HANDLS(I) = 0
         SRCES (I) = 0
         TYPES (I) = ' '

      END DO

!
!     There's just one counter that indicates the number of
!     database entries:  LOADED.  Set this counter to
!     its initial state.
!
      LOADED = 0

!
!     Calling CLPOOL doesn't remove watches, but it does send a message
!     to each agent indicating that its variables have been touched.
!     Clear this indication by calling CVPOOL.  (This is done for
!     safety; the current implementation of FURNSH doesn't require it.)
!
      CALL CVPOOL ( 'FURNSH', UPDATE )


      CALL CHKOUT ( 'KCLEAR' )
      RETURN






!$Procedure UNLOAD ( Unload a kernel )

      ENTRY UNLOAD ( FILE )

! $ Abstract
!
!     Unload a SPICE kernel.
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
!
! $ Declarations
!
!     CHARACTER*(*)         FILE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FILE       I   The name of a kernel to unload.
!
! $ Detailed_Input
!
!     FILE     is the name of a file to unload. This file
!              should be one loaded through the interface FURNSH.
!              If the file is not on the list of loaded kernels
!              no action is taken.
!
!              Note that if FILE is a meta-text kernel, all of
!              the files loaded as a result of loading the meta-text
!              kernel will be unloaded.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the specified kernel is not on the list of loaded kernels
!         no action is taken.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The call
!
!        CALL UNLOAD ( FILE )
!
!     has the effect of "erasing" the last previous call:
!
!        CALL FURNSH ( FILE )
!
!     This interface allows you to unload binary and text kernels.
!     Moreover, if you used a meta-text kernel to set up your
!     working environment, you can unload all of the kernels loaded
!     through the meta-kernel by unloading the meta-kernel.
!
!     The usual usage of FURNSH is to load each file needed by your
!     program exactly one time. However, it is possible to load a
!     kernel more than one time. (Usually, this is a result of loading
!     meta-kernels without taking the care needed to ensure that the
!     meta-kernels do not specify the same file more than once.) The
!     effect of unloading a kernel that has been loaded more than once
!     is to "undo" the last loading of the kernel. Depending upon the
!     kernel and its relationship to other loaded kernels, this may
!     have no visible effect on the working of your program. To
!     illustrate this behavior suppose that you have a collection of
!     files FILE1, FILE2, FILE3, FILE4, FILE5, FILE6, FILE7, FILE8,
!     META1, META2  where FILE1 ... FILE8 are SPICE kernels and META1
!     and META2 are meta-kernels with the specified kernels to load as
!     shown below.
!
!
!         META1:
!            KERNELS_TO_LOAD = ( FILE2,
!                                FILE3,
!                                FILE4,
!                                FILE5 )
!
!         META2:
!            KERNELS_TO_LOAD = ( FILE2,
!                                FILE3,
!                                FILE7,
!                                FILE8 )
!
!
!      The following sequence of calls
!
!          CALL FURNSH ( FILE1 )
!          CALL FURNSH ( FILE2 )
!          CALL FURNSH ( FILE3 )
!          CALL FURNSH ( META1 )
!          CALL FURNSH ( FILE6 )
!          CALL FURNSH ( META2 )
!
!      has the effect:
!
!          "Load" FILE1
!          "Load" FILE2
!          "Load" FILE3
!          "Load" META1 as a text kernel and then...
!                "Load" FILE2 (note that it was loaded from META1)
!                "Load" FILE3 (note that it was loaded from META1)
!                "Load" FILE4 (note that it was loaded from META1)
!                "Load" FILE5 (note that it was loaded from META1)
!          "Load" FILE6
!          "Load" META2 as a text kernel and then...
!                "Load" FILE2 (note that it was loaded from META2)
!                "Load" FILE3 (note that it was loaded from META2) *
!                "Load" FILE7 (note that it was loaded from META2)
!                "Load" FILE8 (note that it was loaded from META2)
!
!      If we  UNLOAD FILE3
!
!         CALL UNLOAD ( FILE3 )
!
!      we locate the last time FILE3 was loaded (* above) and modify the
!      state of loaded kernels so that it looks as if we had made the
!      following sequence of "load" operations.
!
!          "Load" FILE1
!          "Load" FILE2
!          "Load" FILE3
!          "Load" META1 as a text kernel and then...
!                "Load" FILE2 (note that it was loaded from META1)
!                "Load" FILE3 (note that it was loaded from META1)
!                "Load" FILE4 (note that it was loaded from META1)
!                "Load" FILE5 (note that it was loaded from META1)
!          "Load" FILE6
!          "Load" META2 as a text kernel and then...
!                "Load" FILE2 (note that it was loaded from META2)
!                "Load" FILE7 (note that it was loaded from META2)
!                "Load" FILE8 (note that it was loaded from META2)
!
!      As you can see, the data from FILE3 is still available to the
!      program. All that may have changed is the usage priority
!      associated with that data.
!
!      If we unload META2 (or META1) we remove all remaining files that
!      are noted as being loaded from META2 (or META1)
!
!          CALL UNLOAD ( META2 )
!
!      produces the following load state for the program:
!
!          "Load" FILE1
!          "Load" FILE2
!          "Load" FILE3
!          "Load" META1 as a text kernel and then...
!                "Load" FILE2 (note that it was loaded from META1)
!                "Load" FILE3 (note that it was loaded from META1)
!                "Load" FILE4 (note that it was loaded from META1)
!                "Load" FILE5 (note that it was loaded from META1)
!          "Load" FILE6
!
!      If we had unloaded META1 instead, we would have this load state.
!
!          "Load" FILE1
!          "Load" FILE2
!          "Load" FILE3
!          "Load" FILE6
!          "Load" META2 as a text kernel and then...
!                "Load" FILE2 (note that it was loaded from META2)
!                "Load" FILE7 (note that it was loaded from META2)
!                "Load" FILE8 (note that it was loaded from META2)
!
!      So we see that unloading a file does not necessarily make its
!      data unavailable to your program. Unloading modifies the
!      precedence of the files loaded in your program. The data
!      associated with an unloaded file becomes unavailable only when
!      the file has been unloaded as many times as it was loaded.
!
!      When would you encounter such a scenario? The situation of
!      loading a file more than once might appear if you were trying to
!      contrast the results of computations performed with two
!      different meta-kernels. In such a scenario you might load a
!      "baseline" set of kernels early in your program and then load
!      and unload meta-kernels to compare results between the two
!      different sets of data.
!
!     Unloading Text Kernels or Meta-Kernels
!     --------------------------------------
!
!     Part of the action of unloading text (or meta-kernels) is
!     the clearing of the kernel pool and re-loading any kernels that
!     were not in the specified set of kernels to unload. Since
!     loading of text kernels is not a very fast process, unloading
!     text kernels takes considerably longer than unloading binary
!     kernels. Moreover, since the kernel pool is cleared, any kernel
!     pool variables you have set from your program by using one of the
!     interfaces PCPOOL, PDPOOL, PIPOOL, or LMPOOL will be removed from
!     the kernel pool. For this reason, if you plan to use this
!     feature in your program, together with one of the routines
!     specified above, you will need to take special precautions to
!     make sure kernel pool variables required by your program do not
!     inadvertently disappear.
!
!     As a side effect of unloading a text kernel, all watched kernel
!     variables are marked as updated. Note that unloading a text
!     kernel does not delete watchers. Watchers can be deleted by
!     calling the POOL entry point DWPOOL.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Load a meta-kernel with a PCK, an LSK and an SPK, and
!        separately, a text kernel and a binary PCK. Loop over the
!        loaded kernels, outputting file information for each of
!        them.
!
!        Then unload the text kernels, check that they have been
!        unloaded, and finally unload the meta-kernel.
!
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: unload_ex1.tm
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
!              File name                     Contents
!              ---------                     --------
!              de421.bsp                     Planetary ephemeris
!              pck00009.tpc                  Planet orientation and
!                                            radii
!              naif0012.tls                  Leapseconds
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'de421.bsp',
!                                  'naif0012.tls',
!                                  'pck00009.tpc' )
!
!           \begintext
!
!           End of meta-kernel
!
!
!        Use the PCK kernel below as the binary PCK required for the
!        example.
!
!           earth_latest_high_prec.bpc
!
!
!        Use the FK kernel below as the text kernel required for the
!        example.
!
!           RSSD0002.TF
!
!
!        Example code begins here.
!
!
!              PROGRAM UNLOAD_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local constants.
!        C
!              INTEGER               FNAMLN
!              PARAMETER           ( FNAMLN = 256 )
!
!              INTEGER               FTYPLN
!              PARAMETER           ( FTYPLN = 33 )
!
!              INTEGER               SRCLEN
!              PARAMETER           ( SRCLEN = 256 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(FNAMLN)    FILE
!              CHARACTER*(FTYPLN)    FILTYP
!              CHARACTER*(SRCLEN)    SRCFIL
!
!              INTEGER               COUNT
!              INTEGER               HANDLE
!
!              LOGICAL               FOUND
!
!        C
!        C     Load several kernel files.
!        C
!              CALL FURNSH ( 'unload_ex1.tm'              )
!              CALL FURNSH ( 'RSSD0002.TF'                )
!              CALL FURNSH ( 'earth_latest_high_prec.bpc' )
!
!        C
!        C     Count the number of loaded kernel files.
!        C
!              CALL KTOTAL ( 'ALL', COUNT )
!
!              WRITE(*,'(A,I2)') 'The total number of kernels after '
!             . //               'final FURNSH: ', COUNT
!              WRITE(*,*) ' '
!
!        C
!        C     Unload the text kernels.
!        C
!              CALL KTOTAL ( 'TEXT', COUNT )
!
!              WRITE(*,*) ' '
!              WRITE(*,'(A,I2,A)') 'Unloading', COUNT,
!             .                    ' text kernels...'
!              WRITE(*,*) ' '
!
!              DO WHILE ( COUNT .NE. 0 )
!
!                 CALL KDATA (      1, 'TEXT',  FILE, FILTYP,
!             .                SRCFIL, HANDLE, FOUND        )
!
!        C
!        C        If the kernel is found in the pool, unload it.
!        C
!                 IF ( FOUND ) THEN
!
!                    CALL UNLOAD ( FILE )
!
!        C
!        C           Check if the file has been unloaded.
!        C
!                    CALL KINFO ( FILE, FILTYP, SRCFIL, HANDLE, FOUND )
!
!                    IF ( FOUND ) THEN
!
!                       WRITE(*,'(A)') '  Error unloading ' // FILE
!
!                    ELSE
!
!                       WRITE(*,'(A)') '  Success unloading ' // FILE
!
!                    END IF
!
!        C
!        C        Something is not working. Inform NAIF.
!        C
!                 ELSE
!
!                    WRITE(*,*) ' ERROR: No kernel found but KTOTAL '
!             .      //         'returns ', COUNT
!
!                 END IF
!
!        C
!        C        Check if we have more text kernels to unload from
!        C        the kernel pool. Note that unloading a text kernel
!        C        or meta-kernel implies that the kernel pool is
!        C        cleared, and any kernel(s) that were not to be
!        C        unloaded are re-loaded. Therefore the COUNT value
!        C        changes, and the indexing of the files within the
!        C        kernel pool too.
!        C
!                 CALL KTOTAL ( 'TEXT', COUNT )
!
!              END DO
!
!              CALL KTOTAL ( 'ALL', COUNT )
!
!              WRITE(*,*) ' '
!              WRITE(*,'(A,I2)') 'The total number of kernels after '
!             . //               'UNLOAD calls: ', COUNT
!
!        C
!        C     Unload the meta-kernel and retrieve the number of loaded
!        C     after the clear.
!        C
!              CALL UNLOAD ( 'unload_ex1.tm' )
!
!              CALL KTOTAL ( 'ALL', COUNT )
!
!              WRITE(*,*) ' '
!              WRITE(*,'(A,I2)') 'The total number of kernels after '
!             . //               'final UNLOAD: ', COUNT
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        The total number of kernels after final FURNSH:  6
!
!
!        Unloading 3 text kernels...
!
!          Success unloading naif0012.tls
!          Success unloading pck00009.tpc
!          Success unloading RSSD0002.TF
!
!        The total number of kernels after UNLOAD calls:  3
!
!        The total number of kernels after final UNLOAD:  1
!
!
! $ Restrictions
!
!     1)  See the note regarding the unloading of Text and meta-text
!         Kernels.
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
!
! $ Version
!
!-    SPICELIB Version 5.0.1, 08-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!        Created complete code example from existing code fragments.
!
!-    SPICELIB Version 5.0.0, 01-FEB-2017 (NJB) (BVS)
!
!        Updated to support use of DSKs.
!
!        Bug fix: now unloads binary kernels via low-level
!        unload routines only when those kernels have just
!        one entry in the KEEPER database.
!
!        Updated the $Author_and_Institution section.
!
!-    SPICELIB Version 3.0.1, 01-JUL-2014 (NJB)
!
!        Updated discussion of kernel variable watchers.
!
!-    SPICELIB Version 3.0.0, 15-NOV-2006 (NJB)
!
!        Bug fix: corrected update of source pointers when a
!        meta-kernel is unloaded. Previously source pointers
!        having higher indices than those of the files referenced
!        by the meta kernel were not adjusted when the database
!        was compressed.
!
!-    SPICELIB Version 2.0.0, 23-AUG-2001 (WLT)
!
!        Added code to make sure that UNLOAD has the effect of
!        loading all remaining kernels in the order they were first
!        introduced.
!
!-    SPICELIB Version 1.0.0, 01-JUL-1999 (WLT)
!
!-&


! $ Index_Entries
!
!     Unload a SPICE kernel
!
!-&


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'UNLOAD' )


      DIDSPK = .FALSE.
      DIDPCK = .FALSE.
      DIDCK  = .FALSE.
      DIDEK  = .FALSE.
      DIDDSK = .FALSE.
      DIDTXT = .FALSE.


!
!     First locate the file we need to unload, we search backward
!     through the list of loaded files so that we unload in the right
!     order.
!
      GOTIT = .FALSE.
      I     = LOADED

      DO WHILE ( .NOT. GOTIT .AND. I .GT. 0 )

         IF ( FILES(I) .EQ. FILE ) THEN
            GOTIT = .TRUE.
         ELSE
            I     = I - 1
         END IF

      END DO

!
!     If we didn't locate the requested file, there is nothing to do.
!
      IF ( .NOT. GOTIT ) THEN
         CALL CHKOUT ( 'UNLOAD' )
         RETURN
      END IF

!
!     We need to know what type of file we've got so that we
!     can take the correct "unload" action.
!
!     If the kernel to be unloaded is binary, found out how
!     many instances of it are present in the database.
!
!     We take advantage of the fact that all binary kernels
!     use the handle manager subsystem: handles are unique
!     across all file types. We don't need to rely on file
!     names.
!
      IF (    ( TYPES(I) .EQ. 'SPK' ) &
          .OR.( TYPES(I) .EQ. 'CK'  ) &
          .OR.( TYPES(I) .EQ. 'DSK' ) &
          .OR.( TYPES(I) .EQ. 'EK'  ) &
          .OR.( TYPES(I) .EQ. 'PCK' ) ) THEN
!
!        Count the occurrences of the file in the database.
!        Stop if we reach two occurrences.
!
         NMULT = 0
         J     = 1

         DO WHILE ( ( J .LE. LOADED ) .AND. ( NMULT .LT. 2 ) )

            IF ( HANDLS(J) .EQ. HANDLS(I) ) THEN
!
!              To be safe, make sure we're not looking at
!              a text kernel with a random, matching handle
!              value.
!
               IF (      ( TYPES(J) .NE. 'TEXT' ) &
                   .AND. ( TYPES(J) .NE. 'META' ) ) THEN

                  NMULT = NMULT + 1

               END IF

            END IF

            J = J + 1

         END DO

         SINGLE = NMULT .EQ. 1

      END IF


      IF (      TYPES(I) .EQ. 'SPK' ) THEN

         IF ( SINGLE ) THEN
            CALL SPKUEF ( HANDLS(I) )
         END IF

         DIDSPK = .TRUE.

      ELSE IF ( TYPES(I) .EQ. 'CK' ) THEN

         IF ( SINGLE ) THEN
            CALL CKUPF ( HANDLS(I) )
         END IF

         DIDCK = .TRUE.

      ELSE IF ( TYPES(I) .EQ. 'DSK' ) THEN

         IF ( SINGLE ) THEN
            CALL ZZDSKUSF ( HANDLS(I) )
         END IF

         DIDDSK = .TRUE.

      ELSE IF ( TYPES(I) .EQ. 'PCK' ) THEN

         IF ( SINGLE ) THEN
            CALL PCKUOF ( HANDLS(I) )
         END IF

         DIDPCK = .TRUE.

      ELSE IF ( TYPES(I) .EQ. 'EK' ) THEN

         IF ( SINGLE ) THEN
            CALL EKUEF ( HANDLS(I) )
         END IF

         DIDEK = .TRUE.

      ELSE IF ( TYPES(I) .EQ. 'TEXT' ) THEN

         CALL CLPOOL
         DIDTXT = .TRUE.

      ELSE IF ( TYPES(I) .EQ. 'META' ) THEN
!
!        This is a special case, we need to undo the effect of loading
!        the meta-kernel.  This means we need to unload all kernels
!        that were loaded using this meta-kernel.
!
         DIDTXT = .TRUE.
         SRC    = I


         DO J = LOADED, SRC+1, -1

            IF ( SRCES(J) .EQ. SRC ) THEN
!
!              This file was loaded by the meta-kernel of interest.
!              We only need to unload the binary kernels as we
!              will get rid of all text kernels by clearing the
!              kernel pool.
!
!              See whether the file we're about to process is
!              binary, and if so, count the number of times
!              it appears in the database. We have to repeat
!              this test on each loop pass, since the count
!              may have changed since the last pass.
!
               IF (     ( TYPES(J) .EQ. 'SPK' ) &
                   .OR. ( TYPES(J) .EQ. 'CK'  ) &
                   .OR. ( TYPES(J) .EQ. 'DSK' ) &
                   .OR. ( TYPES(J) .EQ. 'EK'  ) &
                   .OR. ( TYPES(J) .EQ. 'PCK' ) ) THEN
!
!                 Count the occurrences of the file in the database.
!                 Stop if we reach two occurrences.
!
                  NMULT = 0
                  K     = 1

                  DO WHILE (      ( K     .LE. LOADED ) &
                            .AND. ( NMULT .LT. 2      ) )

                     IF ( HANDLS(K) .EQ. HANDLS(J) ) THEN
!
!                       To be safe, make sure we're not looking at a
!                       text kernel with a random, matching handle
!                       value.
!
                        IF (     ( TYPES(K) .NE. 'TEXT' ) &
                           .AND. ( TYPES(K) .NE. 'META' ) ) THEN

                           NMULT = NMULT + 1

                        END IF

                     END IF

                     K = K + 1

                  END DO

                  SINGLE = NMULT .EQ. 1

               END IF


               IF ( TYPES(J) .EQ. 'SPK' ) THEN

                  IF ( SINGLE ) THEN
                     CALL SPKUEF ( HANDLS(J) )
                  END IF

                  DIDSPK = .TRUE.

               ELSE IF ( TYPES(J) .EQ. 'CK' ) THEN

                  IF ( SINGLE ) THEN
                     CALL CKUPF ( HANDLS(J) )
                  END IF

                  DIDCK = .TRUE.

               ELSE IF ( TYPES(J) .EQ. 'DSK' ) THEN

                  IF ( SINGLE ) THEN
                     CALL ZZDSKUSF ( HANDLS(J) )
                  END IF

                  DIDDSK = .TRUE.

               ELSE IF ( TYPES(J) .EQ. 'PCK' ) THEN

                  IF ( SINGLE ) THEN
                     CALL PCKUOF ( HANDLS(J) )
                  END IF

                  DIDPCK = .TRUE.

               ELSE IF ( TYPES(J) .EQ. 'EK' ) THEN

                  IF ( SINGLE ) THEN
                     CALL EKUEF ( HANDLS(J) )
                  END IF

                  DIDEK = .TRUE.

               END IF

               N1 = LOADED
               N2 = LOADED
               N3 = LOADED
               CALL REMLAC ( 1, J, FILES,  N1 )
               CALL REMLAC ( 1, J, TYPES,  N2 )
               CALL REMLAI ( 1, J, SRCES,  N3 )
               CALL REMLAI ( 1, J, HANDLS, LOADED )

!
!              Each time we delete an item from the database, any
!              pointer to a location past the deletion point must be
!              updated to reflect the compression of the database.
!              Files loaded from meta kernels are always recorded
!              in the database *after* their sources, so each pointer
!              value is less than the index at which it occurs.
!              So, we need examine only those entries from index J
!              upwards.
!
               DO K = J, LOADED

                  IF ( SRCES(K) .GT. J ) THEN
!
!                    This pointer is affected by the deletion of
!                    the Jth database entry.
!
                     SRCES(K) = SRCES(K)-1

                  END IF

               END DO

            END IF

         END DO
!
!        Now clear the kernel pool.
!
         CALL CLPOOL

      END IF
!
!     Remove the I'th kernel from our local database.
!
      N1 = LOADED
      N2 = LOADED
      N3 = LOADED
      CALL REMLAC ( 1, I, FILES,  N1 )
      CALL REMLAC ( 1, I, TYPES,  N2 )
      CALL REMLAI ( 1, I, SRCES,  N3 )
      CALL REMLAI ( 1, I, HANDLS, LOADED )

!
!     Update any source pointers affected by the deletion of the Ith
!     database entry.
!
      DO J = I, LOADED

         IF ( SRCES(J) .GT. I ) THEN
!
!           This pointer is affected by the deletion of the Ith
!           database entry.
!
            SRCES(J) = SRCES(J)-1

         END IF

      END DO

!
!     If we unloaded a text kernel, we now need to reload all
!     of the text kernels that were not unloaded.
!
      IF ( DIDTXT ) THEN

         DO I = 1, LOADED

            IF (      TYPES(I) .EQ. 'TEXT' &
                 .OR. TYPES(I) .EQ. 'META' ) THEN

               CALL LDPOOL ( FILES(I) )

               IF ( TYPES(I) .EQ. 'META' ) THEN
!
!                 Clean up any debris that may have been left lying
!                 around because we reloaded a meta-text kernel.
!
                  DO J = 1, NKNOWN
                     CALL DVPOOL ( KNOWN(J) )
                  END DO

                  CALL CVPOOL ( 'FURNSH', UPDATE )

               END IF

            END IF

         END DO

      END IF

!
!     If any SPK files were unloaded, we need to reload everything
!     to establish the right priority sequence for segments.
!
      IF ( DIDSPK ) THEN

         DO I = 1, LOADED
            IF ( TYPES(I) .EQ. 'SPK' ) THEN
               CALL SPKLEF ( FILES(I), HANDLS(I) )
            END IF
         END DO

      END IF
!
!     If any CK files were unloaded, we need to reload all of the
!     C-kernels to make sure that we have the correct priorities
!     for the remaining C-kernels.
!
      IF ( DIDCK ) THEN

         DO I = 1, LOADED
            IF ( TYPES(I) .EQ. 'CK' ) THEN
               CALL CKLPF ( FILES(I), HANDLS(I) )
            END IF
         END DO

      END IF

!
!     If any DSK files were unloaded, we need to reload the remaining
!     ones to make sure that we have the correct priorities for the
!     remaining DSKs.
!
      IF ( DIDDSK ) THEN

         DO I = 1, LOADED
            IF ( TYPES(I) .EQ. 'DSK' ) THEN
               CALL ZZDSKLSF ( FILES(I), HANDLS(I) )
            END IF
         END DO

      END IF

!
!     If any binary PCK files were unloaded, we need to reload any
!     remaining ones to re-establish the correct priorities for
!     kernels.
!
      IF ( DIDPCK ) THEN

         DO I = 1, LOADED
            IF ( TYPES(I) .EQ. 'PCK' ) THEN
               CALL PCKLOF ( FILES(I), HANDLS(I) )
            END IF
         END DO

      END IF
!
!     Finally, if any E-kernels were unloaded, we reload the remaining
!     kernels to make sure the state is restored to the correct set
!     of loaded kernels.
!
      IF ( DIDEK ) THEN

         DO I = 1, LOADED
            IF ( TYPES(I) .EQ. 'EK' ) THEN
               CALL EKLEF ( FILES(I), HANDLS(I) )
            END IF
         END DO

      END IF

      CALL CHKOUT ( 'UNLOAD' )
      RETURN

      END

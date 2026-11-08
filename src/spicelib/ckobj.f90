!$Procedure CKOBJ ( CK objects )

      SUBROUTINE CKOBJ ( CKFNM, IDS )

! $ Abstract
!
!     Find the set of ID codes of all objects in a specified CK file.
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
!     CELLS
!     CK
!     DAF
!     NAIF_IDS
!     SETS
!
! $ Keywords
!
!     POINTING
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         CKFNM
      INTEGER               IDS ( LBCELL : * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     CKFNM      I   Name of CK file.
!     IDS       I-O  Set of ID codes of objects in CK file.
!
! $ Detailed_Input
!
!     CKFNM    is the name of a C-kernel.
!
!     IDS      is an initialized SPICE set data structure. IDS
!              optionally may contain a set of ID codes on input; on
!              output, the data already present in IDS will be combined
!              with ID code set found for the file CKFNM.
!
!              If IDS contains no data on input, its size and
!              cardinality still must be initialized.
!
! $ Detailed_Output
!
!     IDS      is a SPICE set data structure which contains the union
!              of its contents upon input with the set of ID codes of
!              each object for which pointing data are present in the
!              indicated CK file. The elements of SPICE sets are
!              unique; hence each ID code in IDS appears only once, even
!              if the CK file contains multiple segments for that ID
!              code.
!
!              See the $Examples section below for a complete example
!              program showing how to retrieve the ID codes from IDS.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input file has transfer format, the error
!         SPICE(INVALIDFORMAT) is signaled.
!
!     2)  If the input file is not a transfer file but has architecture
!         other than DAF, the error SPICE(INVALIDARCHTYPE) is signaled.
!
!     3)  If the input file is a binary DAF file of type other than
!         CK, the error SPICE(INVALIDFILETYPE) is signaled.
!
!     4)  If the CK file cannot be opened or read, an error is signaled
!         by a routine in the call tree of this routine.
!
!     5)  If the size of the output set argument IDS is insufficient to
!         contain the actual number of ID codes of objects covered by
!         the indicated CK file, an error is signaled by a routine in
!         the call tree of this routine.
!
! $ Files
!
!     This routine reads a C-kernel.
!
! $ Particulars
!
!     This routine provides an API via which applications can determine
!     the set of objects for which there are pointing data in a
!     specified CK file.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!
!     1) Display the interval-level coverage for each object in a
!        specified CK file. Use tolerance of zero ticks. Do not
!        request angular velocity. Express the results in the TDB time
!        system.
!
!        Find the set of objects in the file. Loop over the contents
!        of the ID code set: find the coverage for each item in the
!        set and display the coverage.
!
!
!        Example code begins here.
!
!
!              PROGRAM CKOBJ_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              INTEGER               WNCARD
!              INTEGER               CARDI
!        C
!        C     Local parameters
!        C
!        C
!        C     Declare the coverage window.  Make enough room
!        C     for MAXIV intervals.
!        C
!              INTEGER               FILSIZ
!              PARAMETER           ( FILSIZ = 255 )
!
!              INTEGER               LBCELL
!              PARAMETER           ( LBCELL = -5 )
!
!              INTEGER               MAXIV
!              PARAMETER           ( MAXIV  = 100000 )
!
!              INTEGER               WINSIZ
!              PARAMETER           ( WINSIZ = 2 * MAXIV )
!
!              INTEGER               TIMLEN
!              PARAMETER           ( TIMLEN = 50 )
!
!              INTEGER               MAXOBJ
!              PARAMETER           ( MAXOBJ = 1000 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(FILSIZ)    CKFNM
!              CHARACTER*(FILSIZ)    LSK
!              CHARACTER*(FILSIZ)    SCLK
!              CHARACTER*(TIMLEN)    TIMSTR
!
!              DOUBLE PRECISION      B
!              DOUBLE PRECISION      COVER ( LBCELL : WINSIZ )
!              DOUBLE PRECISION      E
!
!              INTEGER               I
!              INTEGER               IDS   ( LBCELL : MAXOBJ )
!              INTEGER               J
!              INTEGER               NIV
!
!        C
!        C     Load a leapseconds kernel and SCLK kernel for output
!        C     time conversion.  Note that we assume a single
!        C     spacecraft clock is associated with all of the objects
!        C     in the CK.
!        C
!              CALL PROMPT ( 'Name of leapseconds kernel > ', LSK  )
!              CALL FURNSH ( LSK )
!
!              CALL PROMPT ( 'Name of SCLK kernel        > ', SCLK )
!              CALL FURNSH ( SCLK )
!
!        C
!        C     Get name of CK file.
!        C
!              CALL PROMPT ( 'Name of CK file            > ', CKFNM )
!
!        C
!        C     Initialize the set IDS.
!        C
!              CALL SSIZEI ( MAXOBJ, IDS )
!
!        C
!        C     Initialize the window COVER.
!        C
!              CALL SSIZED ( WINSIZ, COVER )
!
!        C
!        C     Find the set of objects in the CK file.
!        C
!              CALL CKOBJ ( CKFNM, IDS )
!
!        C
!        C     We want to display the coverage for each object.  Loop
!        C     over the contents of the ID code set, find the coverage
!        C     for each item in the set, and display the coverage.
!        C
!              DO I = 1, CARDI( IDS )
!        C
!        C        Find the coverage window for the current
!        C        object. Empty the coverage window each time
!        C        so we don't include data for the previous object.
!        C
!                 CALL SCARDD ( 0,   COVER )
!                 CALL CKCOV  ( CKFNM,       IDS(I),  .FALSE.,
!             .                 'INTERVAL',  0.D0,    'TDB',    COVER )
!
!        C
!        C        Get the number of intervals in the coverage
!        C        window.
!        C
!                 NIV = WNCARD( COVER )
!
!        C
!        C        Display a simple banner.
!        C
!                 WRITE (*,*) '======================================='
!                 WRITE (*,*) 'Coverage for object ', IDS(I)
!
!        C
!        C        Convert the coverage interval start and stop
!        C        times to TDB calendar strings.
!        C
!                 DO J = 1, NIV
!        C
!        C           Get the endpoints of the Jth interval.
!        C
!                    CALL WNFETD ( COVER, J, B, E )
!        C
!        C           Convert the endpoints to TDB calendar
!        C           format time strings and display them.
!        C
!                    CALL TIMOUT ( B,
!             .                    'YYYY MON DD HR:MN:SC.###### ' //
!             .                    '(TDB) ::TDB',
!             .                    TIMSTR                           )
!                    WRITE (*,*) ' '
!                    WRITE (*,*) 'Interval: ', J
!                    WRITE (*,*) 'Start:    ', TIMSTR
!
!                    CALL TIMOUT ( E,
!             .                    'YYYY MON DD HR:MN:SC.###### ' //
!             .                    '(TDB) ::TDB',
!             .                    TIMSTR                          )
!                    WRITE (*,*) 'Stop:     ', TIMSTR
!                    WRITE (*,*) ' '
!
!                 END DO
!
!                 WRITE (*,*) '======================================='
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, using the LSK file named naif0010.tls, the SCLK file
!        named cas00145.tsc and the CK file named 08052_08057ra.bc, the
!        output was:
!
!
!        Name of leapseconds kernel > naif0010.tls
!        Name of SCLK kernel        > cas00145.tsc
!        Name of CK file            > 08052_08057ra.bc
!         =======================================
!         Coverage for object       -82000
!
!         Interval:            1
!         Start:    2008 FEB 21 00:01:07.771186 (TDB)
!         Stop:     2008 FEB 23 22:53:30.001738 (TDB)
!
!
!         Interval:            2
!         Start:    2008 FEB 23 22:58:13.999732 (TDB)
!         Stop:     2008 FEB 24 02:22:25.913175 (TDB)
!
!
!         Interval:            3
!         Start:    2008 FEB 24 02:27:49.910886 (TDB)
!         Stop:     2008 FEB 24 19:46:33.470587 (TDB)
!
!
!         Interval:            4
!         Start:    2008 FEB 24 19:49:33.469315 (TDB)
!         Stop:     2008 FEB 25 04:25:21.250677 (TDB)
!
!
!         Interval:            5
!         Start:    2008 FEB 25 04:29:33.248897 (TDB)
!         Stop:     2008 FEB 25 15:23:44.971594 (TDB)
!
!
!         Interval:            6
!         Start:    2008 FEB 25 15:24:12.971396 (TDB)
!         Stop:     2008 FEB 25 20:25:04.843864 (TDB)
!
!
!         Interval:            7
!         Start:    2008 FEB 25 20:25:48.843553 (TDB)
!         Stop:     2008 FEB 26 00:01:04.752306 (TDB)
!
!         =======================================
!
!
! $ Restrictions
!
!     1)  If an error occurs while this routine is updating the set
!         IDS, the set may be corrupted.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 08-OCT-2021 (JDR) (NJB)
!
!        Changed input argument name "CK" to "CKFNM" for consistency
!        with other routines.
!
!        Bug fix: added call to FAILED after call to GETFAT.
!
!        Edited the header comments to comply with NAIF standard. Added
!        solution using CASSINI data. Corrected short error message in
!        entries #2 and #3 in $Exceptions section.
!
!-    SPICELIB Version 1.0.1, 30-NOV-2007 (NJB)
!
!        Corrected bug in program in header $Examples section: program
!        now empties the coverage window prior to collecting data for
!        the current object. Deleted declaration of unused parameter
!        NAMLEN in example program. Updated example to use WNCARD
!        rather than CARDD.
!
!-    SPICELIB Version 1.0.0, 30-DEC-2004 (NJB)
!
!-&


! $ Index_Entries
!
!     find id codes of objects in CK file
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               ND
      PARAMETER           ( ND     = 2 )

      INTEGER               NI
      PARAMETER           ( NI     = 6 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    ARCH
      CHARACTER*(LNSIZE)    KERTYP

      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      DESCR  ( ND + NI/2 )

      INTEGER               HANDLE
      INTEGER               IC     ( NI )

      LOGICAL               FOUND

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'CKOBJ' )

!
!     See whether GETFAT thinks we've got a CK file.
!
      CALL GETFAT ( CKFNM, ARCH, KERTYP )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'CKOBJ' )
         RETURN
      END IF

      IF ( ARCH .EQ. 'XFR' ) THEN

         CALL SETMSG ( 'Input file # has architecture #. The file ' &
         //            'must be a binary CK file to be readable ' &
         //            'by this routine.  If the input file is an ' &
         //            'CK file in transfer format, run TOBIN on ' &
         //            'the file to convert it to binary format.'   )
         CALL ERRCH  ( '#',  CKFNM                                  )
         CALL ERRCH  ( '#',  ARCH                                   )
         CALL SIGERR ( 'SPICE(INVALIDFORMAT)'                       )
         CALL CHKOUT ( 'CKOBJ'                                      )
         RETURN

      ELSE IF ( ARCH .NE. 'DAF' ) THEN

         CALL SETMSG ( 'Input file # has architecture #. The file ' &
         //            'must be a binary CK file to be readable ' &
         //            'by this routine.  Binary CK files have ' &
         //            'DAF architecture.  If you expected the ' &
         //            'file to be a binary CK file, the problem ' &
         //            'may be due to the file being an old ' &
         //            'non-native file lacking binary file format ' &
         //            'information. It''s also possible the file ' &
         //            'has been corrupted.'                        )
         CALL ERRCH  ( '#',  CKFNM                                  )
         CALL ERRCH  ( '#',  ARCH                                   )
         CALL SIGERR ( 'SPICE(INVALIDARCHTYPE)'                     )
         CALL CHKOUT ( 'CKOBJ'                                      )
         RETURN

      ELSE IF ( KERTYP .NE. 'CK' ) THEN

         CALL SETMSG ( 'Input file # has file type #. The file ' &
         //            'must be a binary CK file to be readable ' &
         //            'by this routine. If you expected the ' &
         //            'file to be a binary CK file, the problem ' &
         //            'may be due to the file being an old ' &
         //            'non-native file lacking binary file format ' &
         //            'information. It''s also possible the file ' &
         //            'has been corrupted.'                        )
         CALL ERRCH  ( '#',  CKFNM                                  )
         CALL ERRCH  ( '#',  KERTYP                                 )
         CALL SIGERR ( 'SPICE(INVALIDFILETYPE)'                     )
         CALL CHKOUT ( 'CKOBJ'                                      )
         RETURN

      END IF

!
!     Open the file for reading.
!
      CALL DAFOPR ( CKFNM, HANDLE )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'CKOBJ' )
         RETURN

      END IF

!
!     We will examine each segment descriptor in the file, and
!     we'll update our ID code set according to the data found
!     in these descriptors.
!
!     Start a forward search.
!
      CALL DAFBFS ( HANDLE )

!
!     Find the next DAF array.
!
      CALL DAFFNA ( FOUND )

      DO WHILE (  FOUND  .AND.  ( .NOT. FAILED() )  )

!
!        Fetch and unpack the segment descriptor.
!
         CALL DAFGS ( DESCR )
         CALL DAFUS ( DESCR, ND, NI, DC, IC )

!
!        Insert the current ID code into the output set.
!        The insertion algorithm will handle duplicates; no special
!        action is required here.
!
         CALL INSRTI ( IC(1), IDS )

         CALL DAFFNA ( FOUND )

      END DO

!
!     Release the file.
!
      CALL DAFCLS ( HANDLE )

      CALL CHKOUT ( 'CKOBJ' )
      RETURN
      END

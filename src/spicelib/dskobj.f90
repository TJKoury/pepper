!$Procedure DSKOBJ ( DSK, get object IDs )

      SUBROUTINE DSKOBJ ( DSKFNM, BODIDS )

! $ Abstract
!
!     Find the set of body ID codes of all objects for which
!     topographic data are provided in a specified DSK file.
!
! $ Required_Reading
!
!     CELLS
!     DAS
!     DSK
!     SETS
!     NAIF_IDS
!
! $ Keywords
!
!     COVERAGE
!     TOPOGRAPHY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'dla.inc'
      INCLUDE 'dskdsc.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         DSKFNM
      INTEGER               BODIDS ( LBCELL : * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     DSKFNM     I   Name of DSK file.
!     BODIDS    I-O  Set of ID codes of objects in DSK file.
!
! $ Detailed_Input
!
!     DSKFNM   is the name of a DSK file. This file will be opened for
!              read access by this routine.
!
!     BODIDS   is an initialized SPICE set data structure.
!
!              BODIDS optionally may contain a set of body ID codes on
!              input; on output, the data already present in BODIDS will
!              be combined with ID code set found for the file DSKFNM.
!
!              If BODIDS contains no data on input, its size and
!              cardinality still must be initialized.
!
! $ Detailed_Output
!
!     BODIDS   is a SPICE set data structure that contains the union
!              of its contents upon input with the set of body ID codes
!              of segments in the indicated DSK file.
!
!              The elements of SPICE sets are unique; each ID code in
!              BODIDS appears only once, even if the DSK file contains
!              multiple segments for that ID code.
!
!              See the $Examples section below for a complete example
!              program showing how to retrieve the body and surface ID
!              codes from a DSK file.
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
!         other than DAS, the error SPICE(INVALIDARCHTYPE) is signaled.
!
!     3)  If the input file is a binary DAS file of type other than DSK,
!         the error SPICE(INVALIDFILETYPE) is signaled.
!
!     4)  If the DSK file cannot be opened or read, an error is signaled
!         by a routine in the call tree of this routine.
!
!     5)  If the size of the output set argument BODIDS is insufficient
!         to contain the actual number of ID codes of objects covered by
!         the indicated DSK file, the error SPICE(CELLTOOSMALL) is
!         signaled.
!
! $ Files
!
!     See the description of the argument DSKFNM above.
!
! $ Particulars
!
!     This routine provides an API via which applications can determine
!     the set of objects for which there are topographic data in a
!     specified DSK file.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Display the coverage for each object in a specified DSK file.
!        Find the set of objects in the file. Loop over the contents
!        of the ID code set: find the surface ID for each item in the
!        set and display the surface ID.
!
!
!        Example code begins here.
!
!
!        C
!        C     Examine a DSK file and identify the set of
!        C     central bodies associated with the segments
!        C     in the file. For each body, find the
!        C     set of surfaces associated with that body.
!        C
!              PROGRAM DSKOBJ_EX1
!              IMPLICIT NONE
!        C
!        C     SPICELIB functions
!        C
!              INTEGER               CARDI
!        C
!        C     Local parameters
!        C
!              INTEGER               LBCELL
!              PARAMETER           ( LBCELL = -5 )
!
!              INTEGER               FILSIZ
!              PARAMETER           ( FILSIZ = 255 )
!
!              INTEGER               MAXID
!              PARAMETER           ( MAXID  = 10000 )
!        C
!        C     Local variables
!        C
!              CHARACTER*(FILSIZ)    DSKFNM
!
!              INTEGER               BODIDS ( LBCELL : MAXID )
!              INTEGER               I
!              INTEGER               J
!              INTEGER               SRFIDS ( LBCELL : MAXID )
!
!        C
!        C     Initialize body ID and surface ID cells.
!        C
!              CALL SSIZEI ( MAXID, BODIDS )
!              CALL SSIZEI ( MAXID, SRFIDS )
!
!        C
!        C     Prompt for the name of a DSK file.
!        C
!              CALL PROMPT ( 'Enter name of DSK file > ', DSKFNM )
!
!        C
!        C     Obtain body ID set for the DSK.
!        C
!              CALL DSKOBJ ( DSKFNM, BODIDS )
!
!              DO I = 1, CARDI( BODIDS )
!
!                 WRITE (*,*) ' '
!                 WRITE (*,*) 'Body ID:     ', BODIDS(I)
!        C
!        C        Get the surface IDs for the Ith body.
!        C
!                 CALL DSKSRF ( DSKFNM, BODIDS(I), SRFIDS )
!
!                 DO J = 1, CARDI( SRFIDS )
!                    WRITE (*,*) '   Surface ID: ', SRFIDS(J)
!                 END DO
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, using the DSK file named phobos512.bds, the output
!        was:
!
!
!        Enter name of DSK file > phobos512.bds
!
!         Body ID:              401
!            Surface ID:          401
!
!
! $ Restrictions
!
!     1)  If an error occurs while this routine is updating the set
!         BODIDS, the set may be corrupted.
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
!        Changed input argument name "DSK" to "DSKFNM" for consistency
!        with other routines.
!
!        Bug fix: added call to FAILED after call to GETFAT.
!
!        Edited the header comments to comply with NAIF standard.
!        Changed the DKS file used in the code example's solution.
!        Added NAIF_IDS to the $Required_Reading section.
!
!        Corrected short error message in entries #2, #3 and #5 in
!        $Exceptions section.
!
!-    SPICELIB Version 1.0.0, 22-AUG-2016 (NJB)
!
!-&


! $ Index_Entries
!
!     find id codes of ephemeris objects in DSK file
!     find id codes of bodies in DSK file
!
!-&


!
!     SPICELIB functions
!
      INTEGER               CARDI
      INTEGER               SIZEI

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               TOKLEN
      PARAMETER           ( TOKLEN = 4 )

!
!     Local variables
!
      CHARACTER*(TOKLEN)    ARCH
      CHARACTER*(TOKLEN)    KERTYP

      DOUBLE PRECISION      DSKDSC ( DSKDSZ )

      INTEGER               BID
      INTEGER               DLADSC ( DLADSZ )
      INTEGER               HANDLE
      INTEGER               NXTDSC ( DLADSZ )

      LOGICAL               FOUND


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'DSKOBJ' )

!
!     See whether GETFAT thinks we've got a DSK file.
!
      CALL GETFAT ( DSKFNM, ARCH, KERTYP )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DSKOBJ' )
         RETURN
      END IF

      IF ( ARCH .EQ. 'XFR' ) THEN

         CALL SETMSG ( 'Input file # has architecture #. The file ' &
         //            'must be a binary DSK file to be readable ' &
         //            'by this routine. If the input file is an ' &
         //            'DSK file in transfer format, run TOBIN on ' &
         //            'the file to convert it to binary format.'  )
         CALL ERRCH  ( '#',  DSKFNM                                )
         CALL ERRCH  ( '#',  ARCH                                  )
         CALL SIGERR ( 'SPICE(INVALIDFORMAT)'                      )
         CALL CHKOUT ( 'DSKOBJ'                                    )
         RETURN

      ELSE IF ( ARCH .NE. 'DAS' ) THEN

         CALL SETMSG ( 'Input file # has architecture #. The file ' &
         //            'must be a binary DSK file to be readable ' &
         //            'by this routine. Binary DSK files have ' &
         //            'DAS architecture. If you expected the ' &
         //            'file to be a binary DSK file, the problem ' &
         //            'may be due to the file being an old ' &
         //            'non-native file lacking binary file format ' &
         //            'information. It''s also possible the file ' &
         //            'has been corrupted.'                       )
         CALL ERRCH  ( '#',  DSKFNM                                )
         CALL ERRCH  ( '#',  ARCH                                  )
         CALL SIGERR ( 'SPICE(INVALIDARCHTYPE)'                    )
         CALL CHKOUT ( 'DSKOBJ'                                    )
         RETURN

      ELSE IF ( KERTYP .NE. 'DSK' ) THEN

         CALL SETMSG ( 'Input file # has file type #. The file ' &
         //            'must be a binary DSK file to be readable ' &
         //            'by this routine. If you expected the ' &
         //            'file to be a binary DSK file, the problem ' &
         //            'may be due to the file being an old ' &
         //            'non-native file lacking binary file format ' &
         //            'information. It''s also possible the file ' &
         //            'has been corrupted.'                       )
         CALL ERRCH  ( '#',  DSKFNM                                )
         CALL ERRCH  ( '#',  KERTYP                                )
         CALL SIGERR ( 'SPICE(INVALIDFILETYPE)'                    )
         CALL CHKOUT ( 'DSKOBJ'                                    )
         RETURN

      END IF

!
!     Open the DSK for read access; start a forward search.
!
      CALL DASOPR ( DSKFNM, HANDLE )
      CALL DLABFS ( HANDLE, NXTDSC, FOUND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DSKOBJ' )
         RETURN
      END IF

      DO WHILE (  FOUND  .AND.  ( .NOT. FAILED() )  )
!
!        Get the DSK descriptor of the current segment.
!        This is where we'll find the body ID code.
!
         CALL MOVEI ( NXTDSC, DLADSZ, DLADSC )

         CALL DSKGD ( HANDLE, DLADSC, DSKDSC )
!
!        The body ID is at location CTRIDX ("center index")
!        of the DSK descriptor.
!
!        Append, rather than insert, the new ID. We'll
!        turn the cell into a set at the end of the loop.
!
!        Before appending, make sure there's room in the cell
!        for another entry. We can't afford to let APPNDI
!        catch an out-of-room error, because we would lose
!        the ability to close the file.
!
         BID = NINT( DSKDSC(CTRIDX) )

         IF ( CARDI(BODIDS) .EQ. SIZEI(BODIDS) ) THEN
!
!           We're going to signal an error. Close the DSK
!           first.
!
            CALL DSKCLS ( HANDLE, .FALSE. )

            CALL SETMSG ( 'Cannot append body ID # to cell ' &
            //            'while reading DSK file #. ' &
            //            'Cell size is #.'                 )
            CALL ERRINT ( '#', BID                          )
            CALL ERRCH  ( '#', DSKFNM                       )
            CALL ERRINT ( '#', SIZEI( BODIDS )              )
            CALL SIGERR ( 'SPICE(CELLTOOSMALL)'             )
            CALL CHKOUT ( 'DSKOBJ'                          )
            RETURN

         END IF

         CALL APPNDI ( BID, BODIDS )
!
!        Fetch the DLA descriptor of the next segment.
!
         CALL DLAFNS ( HANDLE, DLADSC, NXTDSC, FOUND )

      END DO

      CALL VALIDI ( SIZEI(BODIDS), CARDI(BODIDS), BODIDS )

      CALL DASCLS ( HANDLE )

      CALL CHKOUT ( 'DSKOBJ' )
      RETURN
      END

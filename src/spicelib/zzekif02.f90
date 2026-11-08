!$Procedure ZZEKIF02 ( EK, initialize type 2 segment for fast load )

      SUBROUTINE ZZEKIF02 ( HANDLE, SEGNO )

! $ Abstract
!
!     Initialize a new type 2 EK segment to allow fast loading.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!
! $ Declarations


      INCLUDE 'ekclas07.inc'
      INCLUDE 'ekclas08.inc'
      INCLUDE 'ekclas09.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekpage.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGNO

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGNO      I   Segment number.
!
! $ Detailed_Input
!
!     HANDLE         is the handle of an EK file open for write access.
!                    A new type 2 segment is to be created in this file
!                    via a fast load.  The segment's metadata has
!                    already been set up by EKBSEG.
!
!     SEGNO          is the number of the segment to prepare for a
!                    fast load.
!
! $ Detailed_Output
!
!     None.  See $Particulars for a description of the effect of this
!     routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, the error will be diagnosed by routines
!         called by this routine.
!
!     2)  If an I/O error occurs while reading or writing the indicated
!         file, the error will be diagnosed by routines called by this
!         routine.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine carries out the type-2-specific preparation for
!     populating a type 2 EK segment with data via the fast column
!     loader routines.  This routine expects the segment's metadata to
!     already have been written by EKBSEG.
!
!     This routine expects the segment to contain columns having class
!     7, 8, or 9.
!
! $ Examples
!
!     See EKIFLD.
!
! $ Restrictions
!
!     1)  Assumes total number of words required for column metadata
!         is no greater than IPSIZE.  Currently, with a maximum of 100
!         columns and a maximum metadata size of 2 words per column,
!         this condition is met.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 09-NOV-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               BASE
      INTEGER               CLASS
      INTEGER               DSCBAS
      INTEGER               I
      INTEGER               MBASE
      INTEGER               NCOLS
      INTEGER               NROWS
      INTEGER               OFFSET
      INTEGER               P
      INTEGER               PAGE   ( PGSIZI )
      INTEGER               SEGDSC ( SDSCSZ )

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKIF02' )
      END IF

!
!     Read in the segment descriptor.
!
      CALL ZZEKMLOC ( HANDLE, SEGNO,   PAGE,         MBASE  )
      CALL DASRDI   ( HANDLE, MBASE+1, MBASE+SDSCSZ, SEGDSC )

      NCOLS  =  SEGDSC(NCIDX)
      NROWS  =  SEGDSC(NRIDX)

!
!     Allocate space for column metadata.  We assume that one page
!     of IPSIZE integers is enough room.
!
      CALL ZZEKAPS ( HANDLE, SEGDSC, INT, .FALSE., P, BASE  )


      OFFSET  =  BASE

      DO I = 1, NCOLS
!
!        Read the class from the descriptor of the Ith column directly
!        from the file.  We'll need the descriptor's address in order to
!        update the descriptor in the file.
!
         DSCBAS  =  MBASE + CDOFF + (I-1)*CDSCSZ

         CALL DASRDI ( HANDLE, DSCBAS+CLSIDX, DSCBAS+CLSIDX, CLASS )

!
!        Update the file.  Set the column descriptor's metadata pointer
!        to the base address of the metadata area.
!
         CALL DASUDI ( HANDLE, DSCBAS+METIDX, DSCBAS+METIDX, OFFSET )

!
!        Increment the metadata offset by the size of the metadata
!        for the current column.  The classes of interest range from
!        7 to 9.
!
         IF ( CLASS .EQ. 7 ) THEN

            OFFSET  =  OFFSET + MDSZ07


         ELSE IF ( CLASS .EQ. 8 ) THEN

            OFFSET  =  OFFSET + MDSZ08


         ELSE IF ( CLASS .EQ. 9 ) THEN

            OFFSET  =  OFFSET + MDSZ09

         ELSE

            CALL SETMSG ( 'Class # is not supported.' )
            CALL ERRINT ( '#',  CLASS                 )
            CALL SIGERR ( 'SPICE(NOCLASS)'            )
            CALL CHKOUT ( 'ZZEKIF02'                  )
            RETURN

         END IF

      END DO


      CALL CHKOUT ( 'ZZEKIF02' )
      RETURN
      END

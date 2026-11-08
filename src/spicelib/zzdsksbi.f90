!$Procedure ZZDSKSBI ( DSK, initialize API segment buffer )

      SUBROUTINE ZZDSKSBI ( MAXBOD, STSIZE, BTBODY, BTNBOD, BTSEGP, &
                            BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                            STOFF,  STCTR,  STRAD                  )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Initialize DSK API segment buffer data structures.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DLA
!     DSK
!     TOPOGRAPHY
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'dla.inc'
      INCLUDE 'dskdsc.inc'

      INTEGER               MAXBOD
      INTEGER               STSIZE
      INTEGER               BTBODY ( * )
      INTEGER               BTNBOD
      INTEGER               BTSEGP ( * )
      INTEGER               BTSTSZ ( * )
      INTEGER               STHAN  ( * )
      DOUBLE PRECISION      STDSCR ( DSKDSZ, * )
      INTEGER               STDLAD ( DLADSZ, * )
      INTEGER               STFREE
      DOUBLE PRECISION      STOFF  ( 3, * )
      DOUBLE PRECISION      STCTR  ( 3, * )
      DOUBLE PRECISION      STRAD  ( * )


! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     MAXBOD     I   Maximum size of body table.
!     STSIZE     I   Maximum size of segment table.
!     BTBODY    I-O  Body table's body ID array.
!     BTNBOD    I-O  Number of entries in body table.
!     BTSEGP    I-O  Array of pointers from bodies to segment table.
!     BTSTSZ    I-O  Array of sizes of body segment lists.
!     STHAN     I-O  Array of handles of segments.
!     STDSCR    I-O  Array of DSK descriptors of segments.
!     STDLAD    I-O  Array of DLA descriptors of segments.
!     STFREE    I-O  Index of first free entry in segment table.
!     STOFF     I-O  Offsets of segment frame centers from body.
!     STCTR     I-O  Centers of bounding spheres for segments.
!     STRAD     I-O  Radii of bounding spheres for segments.
!
! $ Detailed_Input
!
!     MAXBOD     is the maximum size of the body table. BTBODY, BTSEGP,
!                and BTSTSZ must each be declared by the caller with
!                size at least MAXBOD.
!
!     STSIZE     is the size of the segment table. STHAN, STDSCR,
!                STDLAD, STOFF, STCTR, and STRAD must be declared large
!                enough to hold STSIZE entries.
!
!     BTBODY     is a table of body IDs.
!
!     BTNBOD     is the number of body IDs currently in BTBODY.
!
!     BTSTSZ     is an array of segment list sizes. The Ith entry
!                of BTSTSZ is the length of the segment list for the
!                Ith body.
!
!     STHAN      is an array of DAS handles. Each entry of STHAN that
!                is in use contains the handle of the DAS file
!                containing the DSK segment to which that entry
!                corresponds. The Ith entries of STHAN, STDSCR, STDLAD,
!                STOFF, STCTR, and STRAD correspond to the same DSK
!                segment.
!
!     STDSCR     is an array of DSK descriptors.
!
!     STDLAD     is an array of DLA descriptors.
!
!     STFREE     is the index of the first free entry in the segment
!                table.
!
!     STOFF      is an array of offsets of segment frame centers
!                from the central bodies of the segment. These offsets
!                are expressed in the reference frame of the segment.
!                They are constant vectors. Units are km.
!
!     STCTR      is an array of centers of outer bounding spheres for
!                segments. Each segment has an outer bounding sphere
!                that completely encloses that segment. Each center
!                is a vector expressed in the reference frame of the
!                the segment, and it is an offset from the frame's
!                center. Units are km.
!
!     STRAD      is an array of radii of outer bounding spheres of
!                segments. Units are km.
!
!
!
! $ Detailed_Output
!
!     BTBODY     is the input body ID table, zero-filled.
!
!
!     BTNBOD     is the number of bodies in the body table, set to zero.
!
!     BTSTSZ     is the array of sizes of segment lists of bodies,
!                zero-filled.
!
!     STHAN      is the segment handle array, zero-filled.
!
!
!     STDSCR     is the segment DSK descriptor array, zero-filled.
!
!
!     STDLAD     is the segment DLA descriptor array, zero-filled.
!
!
!     STFREE     is the index of the first free element in each
!                segment table array, set to 1.
!
!
!     STOFF      is the segment frame center offset array, zero-filled.
!
!
!     STCTR      is the segment bounding sphere center array,
!                zero-filled.
!
!
!     STRAD      is the segment bounding sphere radius array,
!                zero-filled.
!
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is meant to be used only by the DSK subsystem.
!
!     This routine initializes data structures used by SPICELIB
!     geometry APIs to perform efficient computations using
!     DSK data. The umbrella routine in which these structures
!     are declared is ZZDSKSBF.
!
!     In a sense, ZZDSKSBF sits "above" the ZZDSKBSR subsystem.
!     High-level geometry routines access the data stored in the
!     ZZDSKSBF arrays directly; they don't call entry points of
!     ZZDSKBSR.
!
!     The ZZDSKSBF subsystem maintains two logical tables: a body
!     table and a segment table. Both tables are designed to store
!     a fixed maximum number of items; there is no "virtual storage"
!     concept that applies.
!
!     Items in both tables have priority assigned on a first-in,
!     first-out basis. When it becomes necessary to remove an
!     item to make room for another, the lowest-indexed item is
!     removed first, and the tables are compressed so that the
!     remaining items are contiguous.
!
!     When the state of the underlying ZZDSKBSR system changes due to
!     loading or unloading of DSK files, the ZZDSKSBF body and segment
!     tables must be re-initialized. Entry points of ZZDSKSBF are
!     expected to perform this action by calling this routine.
!
! $ Examples
!
!     See usage in ZZDSKSBF.
!
! $ Restrictions
!
!     1) This is a private routine.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 14-OCT-2021 (BVS)
!
!        Bug fix: fixed routine name in CHKIN/CHKOUT calls
!        (ZZDSKBSI -> ZZDSKSBI).
!
!-    SPICELIB Version 1.0.0, 11-FEB-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     initialize DSK API segment buffer
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               I


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZDSKSBI' )

!
!     Clear the body table.
!
      BTNBOD = 0

      DO I = 1, MAXBOD

         BTBODY(I) = 0
         BTSEGP(I) = 0
         BTSTSZ(I) = 0

      END DO

!
!     Clear the segment table.
!
      DO I = 1, STSIZE

         STHAN(I) = 0

         CALL CLEARD( DSKDSZ, STDSCR(1,I) )
         CALL CLEARI( DLADSZ, STDLAD(1,I) )

         CALL CLEARD( 3,      STOFF (1,I) )
         CALL CLEARD( 3,      STCTR (1,I) )

         STRAD(I) = 0.D0

      END DO


      STFREE = 1

      CALL CHKOUT ( 'ZZDSKSBI' )
      RETURN
      END

!$Procedure T_TOPKER ( Create topocentric FK and SPK )

      SUBROUTINE T_TOPKER ( FK,     SPK,    TARGET, TRGREF, NSITES, &
                            SITIDS, SITNMS, SITPOS, SITFNM, SITFID, &
                            FIRST,  LAST,   AXES,   ANGLES         )

! $ Abstract
!
!     Create a topocentric frame kernel and SPK file for a set of
!     sites on a specified target body.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     FRAME
!     KERNEL
!     SPK
!     TOPOCENTRIC
!     TEST
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         FK
      CHARACTER*(*)         SPK
      CHARACTER*(*)         TARGET
      CHARACTER*(*)         TRGREF
      INTEGER               NSITES
      INTEGER               SITIDS ( NSITES )
      CHARACTER*(*)         SITNMS ( NSITES )
      DOUBLE PRECISION      SITPOS ( 3, NSITES )
      CHARACTER*(*)         SITFNM ( NSITES )
      INTEGER               SITFID ( NSITES )
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      INTEGER               AXES   ( 3, NSITES )
      DOUBLE PRECISION      ANGLES ( 3, NSITES )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FK         I   Name of frame kernel to create.
!     SPK        I   Name of SPK file to create.
!     TARGET     I   Name of target body.
!     TRGREF     I   Target body's dody-fixed frame.
!     NSITES     I   Number of sites.
!     SITIDS     I   Body ID codes of sites.
!     SITNMS     I   Names of sites.
!     SITPOS     I   Positions of sites relative to target.
!     SITFNM     I   Site frame names.
!     SITFID     I   Site frame ID codes.
!     FIRST      I   SPK coverage start time.
!     LAST       I   SPK coverage stop time.
!     AXES       I   Axis indices of Euler angles.
!     ANGLES     I   Euler angles giving frame orientations.
!
! $ Detailed_Input
!
!     FK         is the name of the frame kernel to create. The
!                file must not already exist.
!
!     SPK        is the name of the SPK file to create. The file
!                contains positions the sites relative to the target
!                body (see description of TARGET below). The file must
!                not already exist.
!
!     TARGET     is the name of the target body. The positions
!                of all sites are expressed relative to the
!                position of the target.
!
!     TRGREF     is the name of a body-centered, body-fixed
!                reference frame associated with the target.
!
!     NSITES     is the number of sites for which frames and
!                SPK segments are to be created.
!
!     SITIDS     is an array of body ID codes of the sites.
!
!     SITNMS     is an array of names of the sites.
!
!     SITPOS     is an array of position vectors of the sites. The
!                positions of all sites are expressed relative to the
!                position of the target, in the reference frame
!                designated by TRGREF.
!
!     SITFNM     is an array of names of reference frames associated
!                with the sites.
!
!     SITFID     is an array of frame IDs for the frames associated
!                with the sites.
!
!     FIRST      is the coverage start time of the site position
!                SPK file.
!
!     LAST       is the coverage end time of the site position
!                SPK file.
!
!
!     AXES       is an array of sequences of axes of Euler rotations.
!                Elements
!
!                    AXES(J,I),  J = 1, 3
!
!                are the indices for the Ith site reference frame.
!                See the discussion of ANGLES below.
!
!     ANGLES     is an array of sequences of angles of Euler rotations.
!                Elements
!
!                    ANGLES(J,I),  J = 1, 3
!
!                are the angles for the Ith site reference frame.
!
!                If M is the matrix that converts vectors relative to a
!                site frame to the target frame by left multiplication,
!                then the angles and axes must satisfy the following
!                relationship:
!
!                   M = [angle_1]      [angle_2]      [angle_3]
!                                axis_1         axis_2         axis_3
!
! $ Detailed_Output
!
!     None. This routine creates two files. See Files below.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the site count is non-positive, the error
!         SPICE(INVALIDCOUNT) is signaled.
!
!     2)  If the target body name cannot be converted to
!         an ID code, the error SPICE(IDCODENOTFOUND)
!         is signaled.
!
!     3)  If the file named by FK already exists, the error
!         SPICE(FILEEXISTS) is signaled.
!
!     4)  If an error occurs while writing the SPK file, the
!         error will be diagnosed by a routine in the call
!         tree of this routine.
!
!     5)  If an internal write performed by this routine fails,
!         the error SPICE(WRITEFAILED) will be signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The site reference frames are typically topocentric, but they
!     are not required to be (this is why the caller supplies the
!     site frame orientation).
!
!     To create a frame in the style of NAIF DSN topocentric frames,
!     where the planetodetic longitude and latitude of the site
!     are LON and LAT respectively, use the rotation sequence
!
!           [angle_1]      [angle_2]      [angle_3]
!                    axis_1         axis_2         axis_3
!
!        =
!
!           [ -LON ]  [ LAT - pi/2 ]  [ pi ]
!                   3               2       3
!
! $ Examples
!
!     None.
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    Test utilities version 1.0.0, 16-JUN-2016 (NJB)
!
!-&



!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPR

      INTEGER               RTRIM

      LOGICAL               EXISTS
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      CHARACTER*(*)         FMT1
      PARAMETER           ( FMT1   = '(F18.13)' )

      CHARACTER*(*)         IDWORD
      PARAMETER           ( IDWORD = 'KPL/FK' )

      CHARACTER*(*)         SQUOTE
      PARAMETER           ( SQUOTE = '''' )


      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 40 )

      INTEGER               BSLASH
      PARAMETER           ( BSLASH = 92 )

      INTEGER               OLSIZE
      PARAMETER           ( OLSIZE = 255 )

      INTEGER               MARKLN
      PARAMETER           ( MARKLN = 10 )

      INTEGER               KVNMLN
      PARAMETER           ( KVNMLN = 32 )

      INTEGER               LMARG
      PARAMETER           ( LMARG  = 4 )

      INTEGER               EQIDX
      PARAMETER           ( EQIDX  = 40 )

      INTEGER               VALIDX
      PARAMETER           ( VALIDX = EQIDX  + 3 )

      INTEGER               NUMIDX
      PARAMETER           ( NUMIDX = VALIDX + 2 )

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               NUMLEN
      PARAMETER           ( NUMLEN = 25 )


!
!     Local variables
!
      CHARACTER*(NUMLEN)    ANGSTR ( 3 )
      CHARACTER*(MARKLN)    BEGDAT
      CHARACTER*(MARKLN)    BEGTXT
      CHARACTER*(BDNMLN)    BNAME
      CHARACTER*(KVNMLN)    FSTEM
      CHARACTER*(OLSIZE)    OUTLIN
      CHARACTER*(SIDLEN)    SEGID

      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      STATES ( 6, 2 )
      DOUBLE PRECISION      STEP

      INTEGER               DEGREE
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               J
      INTEGER               NSTATE
      INTEGER               TRGCDE
      INTEGER               UNIT

      LOGICAL               FOUND

!
!     Saved variables
!



      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_TOPKER' )


      IF ( NSITES .LT. 1 ) THEN

         CALL SETMSG ( 'Site count must be positive but was #.' )
         CALL ERRINT ( '#', NSITES                              )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                    )
         CALL CHKOUT ( 'T_TOPKER'                               )
         RETURN

      END IF

!
!     Get the body ID for the target.
!
      CALL BODS2C ( TARGET, TRGCDE, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'Could not translate body name #.' )
         CALL ERRCH  ( '#', TARGET                        )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'            )
         CALL CHKOUT ( 'T_TOPKER'                         )
         RETURN

      END IF

!
!     Create an SPK file containing data for the input sites.
!
      CALL SPKOPN ( SPK, SPK, 0, HANDLE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_TOPKER' )
         RETURN
      END IF

      CALL CLEARD ( 12, STATES )

      SEGID  = ' '
      NSTATE = 2
      DEGREE = 1
      STEP   = LAST - FIRST

      DO I = 1, NSITES

         SEGID = 'Body #'
         CALL REPMI ( SEGID, '#', I, SEGID )

         CALL VEQU ( SITPOS(1,I), STATES(1,1) )
         CALL VEQU ( SITPOS(1,I), STATES(1,2) )

         CALL SPKW08 ( HANDLE, SITIDS(I), TRGCDE, TRGREF, &
                       FIRST,  LAST,      SEGID,  DEGREE, &
                       NSTATE, STATES,    FIRST,  STEP   )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'T_TOPKER' )
            RETURN
         END IF

      END DO

      CALL SPKCLS ( HANDLE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_TOPKER' )
         RETURN
      END IF


!
!     Create frame kernel.
!
      IF ( EXISTS(FK) ) THEN

         CALL SETMSG ( 'Frame kernel # already exists; this ' &
         //            'program wont''t overwrite it.'       )
         CALL ERRCH  ( '#', FK                               )
         CALL SIGERR ( 'SPICE(FILEEXISTS)'                   )
         CALL CHKOUT ( 'T_TOPKER'                            )
         RETURN

      END IF

!
!     Open output file.
!
      CALL TXTOPN ( FK, UNIT )

!
!     Write ID word.
!
      CALL WRITLN ( IDWORD, UNIT )

      CALL WRITLN ( ' ',       UNIT )


      OUTLIN = '   FILE: '//FK(:RTRIM(FK))
      CALL WRITLN ( OUTLIN,    UNIT )
      CALL WRITLN ( ' ',       UNIT )


      OUTLIN = '   This file was created by T_TOPKER.'
      CALL WRITLN ( OUTLIN,    UNIT )
      CALL WRITLN ( ' ',       UNIT )


!
!     Initialize data and text markers.
!
      BEGDAT =  CHAR(BSLASH) // 'begindata'
      BEGTXT =  CHAR(BSLASH) // 'begintext'



!
!     Create a body-name mapping for each central body, in case the
!     mapping is not already known to SPICE.
!
      CALL WRITLN ( ' ',    UNIT )

      OUTLIN = '   Body-name mapping follows:'
      CALL WRITLN ( OUTLIN, UNIT )

!
!     Start data area.
!
      CALL WRITLN ( ' ',    UNIT )
      CALL WRITLN ( BEGDAT, UNIT )

      DO I = 1, NSITES

         CALL UCASE( SITNMS(I), BNAME )

         CALL PREFIX ( SQUOTE, 0, BNAME )
         CALL SUFFIX ( SQUOTE, 0, BNAME )

         CALL WRITLN (  ' ', UNIT )

         OUTLIN             = ' '
         OUTLIN ( LMARG: )  = 'NAIF_BODY_NAME'
         OUTLIN ( EQIDX: )  = '+='
         OUTLIN ( VALIDX: ) = BNAME

         CALL WRITLN ( OUTLIN, UNIT )

         OUTLIN            = ' '
         OUTLIN ( LMARG: ) = 'NAIF_BODY_CODE'
         OUTLIN ( EQIDX: ) = '+='

         CALL INTSTR ( SITIDS(I), OUTLIN(VALIDX:) )

         CALL WRITLN ( OUTLIN, UNIT )

      END DO


!
!     End data area.
!
      CALL WRITLN ( ' ',    UNIT )
      CALL WRITLN ( BEGTXT, UNIT )
      CALL WRITLN ( ' ',    UNIT )

!
!     Introduce the frame definitions.
!
      CALL WRITLN ( ' ',    UNIT )

      OUTLIN = '   Reference frame specifications follow:'
      CALL WRITLN ( OUTLIN, UNIT )
      CALL WRITLN ( ' ',    UNIT )
      CALL WRITLN ( ' ',    UNIT )

!
!     Start data area.
!
      CALL WRITLN ( ' ',    UNIT )
      CALL WRITLN ( BEGDAT, UNIT )
      CALL WRITLN ( ' ',    UNIT )


      DO I = 1, NSITES
!
!        Create the frame ID assignment.
!
         OUTLIN = ' '

         CALL LJUCRS ( 0, SITFNM(I), OUTLIN(LMARG:) )

         CALL PREFIX ( 'FRAME_', 0, OUTLIN(LMARG:) )

         OUTLIN(EQIDX:) = '='

         CALL INTSTR ( SITFID(I), OUTLIN(VALIDX:) )

         CALL WRITLN ( OUTLIN, UNIT )

!
!        Create the frame name assignment.
!
         OUTLIN = ' '

         CALL INTSTR ( SITFID(I),   FSTEM )
         CALL PREFIX ( 'FRAME_', 0, FSTEM )

         OUTLIN(LMARG:)  = FSTEM
         CALL SUFFIX ( '_NAME',  0, OUTLIN(LMARG:) )

         OUTLIN(EQIDX:)  = '='

         CALL LJUCRS ( 0, SITFNM(I), OUTLIN(VALIDX:) )

         CALL PREFIX ( SQUOTE, 0, OUTLIN(VALIDX:) )
         CALL SUFFIX ( SQUOTE, 0, OUTLIN(VALIDX:) )

         CALL WRITLN ( OUTLIN, UNIT )

!
!        Create the frame class and class ID assignments.
!
         OUTLIN          = ' '
         OUTLIN(LMARG:)  = FSTEM
         CALL SUFFIX ( '_CLASS',  0, OUTLIN(LMARG:) )

         OUTLIN(EQIDX:)  = '=  4'

         CALL WRITLN ( OUTLIN, UNIT )


         OUTLIN          = ' '
         OUTLIN(LMARG:)  = FSTEM
         CALL SUFFIX ( '_CLASS_ID',  0, OUTLIN(LMARG:) )

         OUTLIN(EQIDX:)  = '='

         CALL INTSTR ( SITFID(I), OUTLIN(VALIDX:) )

         CALL WRITLN ( OUTLIN, UNIT )

!
!        Create the center assignment.
!
         OUTLIN          = ' '
         OUTLIN(LMARG:)  = FSTEM
         CALL SUFFIX ( '_CENTER',  0, OUTLIN(LMARG:) )

         OUTLIN(EQIDX:)  = '='

         CALL INTSTR ( SITIDS(I), OUTLIN(VALIDX:) )

         CALL WRITLN ( OUTLIN, UNIT )

!
!        Create the object-frame association assignment.
!
         OUTLIN          = ' '

         CALL INTSTR ( SITIDS(I),    OUTLIN(LMARG:) )
         CALL PREFIX ( 'OBJECT_', 0, OUTLIN(LMARG:) )
         CALL SUFFIX ( '_FRAME',  0, OUTLIN(LMARG:) )

         OUTLIN(EQIDX:)  = '='

         CALL LJUCRS ( 0, SITFNM(I), OUTLIN(VALIDX:) )

         CALL PREFIX ( SQUOTE, 0, OUTLIN(VALIDX:) )
         CALL SUFFIX ( SQUOTE, 0, OUTLIN(VALIDX:) )

         CALL WRITLN ( ' ',    UNIT )
         CALL WRITLN ( OUTLIN, UNIT )
         CALL WRITLN ( ' ',    UNIT )

!
!        Create the relative frame assignment.
!
         CALL PREFIX ( 'TK', 0, FSTEM )

         OUTLIN         = ' '
         OUTLIN(LMARG:) = FSTEM
         CALL SUFFIX ( '_RELATIVE', 0, OUTLIN(LMARG:) )

         OUTLIN(EQIDX:)  = '='

         OUTLIN(VALIDX:) = TRGREF

         CALL PREFIX ( SQUOTE, 0, OUTLIN(VALIDX:) )
         CALL SUFFIX ( SQUOTE, 0, OUTLIN(VALIDX:) )

         CALL WRITLN ( OUTLIN, UNIT )


!
!        Create the frame and units assignments.
!
         OUTLIN = ' '

         OUTLIN(LMARG:) = FSTEM

         CALL SUFFIX ( '_SPEC', 0, OUTLIN(LMARG:) )

         OUTLIN(EQIDX:)  = '=  ''ANGLES'''

         CALL WRITLN ( OUTLIN, UNIT )

         OUTLIN = ' '

         OUTLIN(LMARG:) = FSTEM

         CALL SUFFIX ( '_UNITS', 0, OUTLIN(LMARG:) )

         OUTLIN(EQIDX:)  = '=  ''DEGREES'''

         CALL WRITLN ( OUTLIN, UNIT )

!
!        Create the axis sequence assignment.
!
         OUTLIN = ' '

         OUTLIN(LMARG:) = FSTEM

         CALL SUFFIX ( '_AXES', 0, OUTLIN(LMARG:) )

         OUTLIN(EQIDX:)  = '='

         OUTLIN(VALIDX:) = '( #, #, # )'

         DO J = 1, 3

            CALL REPMI ( OUTLIN(VALIDX:), '#', AXES(J,I), &
                         OUTLIN(VALIDX:)                  )

         END DO

         CALL WRITLN ( OUTLIN, UNIT )


!
!        Create the angle sequence assignments.
!
!
!        First convert the angles to strings. Scale to degrees
!        in the process.
!
         DO J = 1, 3

            ANGLE = ANGLES(J,I) * DPR()

            WRITE ( ANGSTR(J), FMT1, IOSTAT = IOSTAT )   ANGLE

            IF ( IOSTAT .NE. 0 ) THEN

               CALL SETMSG ( 'Attempt to write angle # for ' &
               //            'frame # failed; IOSTAT was #.'      )
               CALL ERRINT ( '#', J                               )
               CALL ERRCH  ( '#', SITFNM(I)                       )
               CALL ERRINT ( '#', IOSTAT                          )
               CALL SIGERR ( 'SPICE(WRITEFAILED)'                 )
               CALL CHKOUT ( 'T_TOPKER'                           )
               RETURN

            END IF

         END DO

!
!        First angle:
!
         OUTLIN = ' '
         OUTLIN(LMARG:) = FSTEM

         CALL SUFFIX ( '_ANGLES', 0, OUTLIN(LMARG:) )

         OUTLIN(EQIDX:)  = '=  ('

         OUTLIN(NUMIDX:) = ANGSTR(1)

         CALL SUFFIX ( ',', 0, OUTLIN(NUMIDX:) )

         CALL WRITLN ( OUTLIN, UNIT )

!
!        Second angle:
!
         OUTLIN = ' '

         OUTLIN(NUMIDX:) = ANGSTR(2)

         CALL SUFFIX ( ',', 0, OUTLIN(NUMIDX:) )

         CALL WRITLN ( OUTLIN, UNIT )

!
!        Third angle:
!
         OUTLIN = ' '

         OUTLIN(NUMIDX:) = ANGSTR(3)

         CALL SUFFIX ( ')', 1, OUTLIN(NUMIDX:) )

         CALL WRITLN ( OUTLIN, UNIT )

!
!        Add a little vertical white space between frame
!        specifications.
!
         CALL WRITLN ( ' ', UNIT )

      END DO

!
!     End data area.
!
      CALL WRITLN ( BEGTXT, UNIT )
      CALL WRITLN ( ' ',    UNIT )

      CALL WRITLN ( ' ',                          UNIT )
      CALL WRITLN ( '[End of definitions file]',  UNIT )
      CALL WRITLN ( ' ',                          UNIT )

!
!     Close output file.
!
      CLOSE ( UNIT )


      CALL CHKOUT ( 'T_TOPKER' )
      RETURN
      END


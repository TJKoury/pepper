C$Procedure T_TOPKER ( Create topocentric FK and SPK )
 
      SUBROUTINE T_TOPKER ( FK,     SPK,    TARGET, TRGREF, NSITES,
     .                      SITIDS, SITNMS, SITPOS, SITFNM, SITFID,
     .                      FIRST,  LAST,   AXES,   ANGLES         )
 
C$ Abstract
C
C     Create a topocentric frame kernel and SPK file for a set of
C     sites on a specified target body.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     FRAME
C     KERNEL
C     SPK
C     TOPOCENTRIC
C     TEST 
C     UTILITY
C
C$ Declarations
 
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
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FK         I   Name of frame kernel to create.
C     SPK        I   Name of SPK file to create.
C     TARGET     I   Name of target body.
C     TRGREF     I   Target body's dody-fixed frame.
C     NSITES     I   Number of sites.
C     SITIDS     I   Body ID codes of sites.
C     SITNMS     I   Names of sites.
C     SITPOS     I   Positions of sites relative to target.
C     SITFNM     I   Site frame names.
C     SITFID     I   Site frame ID codes.
C     FIRST      I   SPK coverage start time.
C     LAST       I   SPK coverage stop time.
C     AXES       I   Axis indices of Euler angles.
C     ANGLES     I   Euler angles giving frame orientations.
C
C$ Detailed_Input
C
C     FK         is the name of the frame kernel to create. The
C                file must not already exist.
C
C     SPK        is the name of the SPK file to create. The file
C                contains positions the sites relative to the target
C                body (see description of TARGET below). The file must
C                not already exist.
C
C     TARGET     is the name of the target body. The positions
C                of all sites are expressed relative to the 
C                position of the target.
C
C     TRGREF     is the name of a body-centered, body-fixed 
C                reference frame associated with the target.
C
C     NSITES     is the number of sites for which frames and
C                SPK segments are to be created.
C
C     SITIDS     is an array of body ID codes of the sites.
C
C     SITNMS     is an array of names of the sites.
C
C     SITPOS     is an array of position vectors of the sites. The
C                positions of all sites are expressed relative to the
C                position of the target, in the reference frame
C                designated by TRGREF.
C
C     SITFNM     is an array of names of reference frames associated
C                with the sites.
C
C     SITFID     is an array of frame IDs for the frames associated
C                with the sites.
C
C     FIRST      is the coverage start time of the site position
C                SPK file.
C
C     LAST       is the coverage end time of the site position
C                SPK file.
C
C
C     AXES       is an array of sequences of axes of Euler rotations.
C                Elements
C
C                    AXES(J,I),  J = 1, 3
C
C                are the indices for the Ith site reference frame.
C                See the discussion of ANGLES below.
C 
C     ANGLES     is an array of sequences of angles of Euler rotations.
C                Elements
C
C                    ANGLES(J,I),  J = 1, 3
C
C                are the angles for the Ith site reference frame.
C
C                If M is the matrix that converts vectors relative to a
C                site frame to the target frame by left multiplication,
C                then the angles and axes must satisfy the following
C                relationship:
C 
C                   M = [angle_1]      [angle_2]      [angle_3]
C                                axis_1         axis_2         axis_3
C
C$ Detailed_Output
C
C     None. This routine creates two files. See Files below.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the site count is non-positive, the error
C         SPICE(INVALIDCOUNT) is signaled.
C
C     2)  If the target body name cannot be converted to
C         an ID code, the error SPICE(IDCODENOTFOUND)
C         is signaled.
C
C     3)  If the file named by FK already exists, the error
C         SPICE(FILEEXISTS) is signaled.
C
C     4)  If an error occurs while writing the SPK file, the
C         error will be diagnosed by a routine in the call
C         tree of this routine.
C
C     5)  If an internal write performed by this routine fails,
C         the error SPICE(WRITEFAILED) will be signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The site reference frames are typically topocentric, but they
C     are not required to be (this is why the caller supplies the 
C     site frame orientation).
C
C     To create a frame in the style of NAIF DSN topocentric frames,
C     where the planetodetic longitude and latitude of the site 
C     are LON and LAT respectively, use the rotation sequence
C
C           [angle_1]      [angle_2]      [angle_3]
C                    axis_1         axis_2         axis_3
C
C        =
C
C           [ -LON ]  [ LAT - pi/2 ]  [ pi ]
C                   3               2       3
C        
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    Test utilities version 1.0.0, 16-JUN-2016 (NJB)
C
C-&
 
      

C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPR

      INTEGER               RTRIM

      LOGICAL               EXISTS
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
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

 
C
C     Local variables
C
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

C
C     Saved variables
C



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

C
C     Get the body ID for the target.
C 
      CALL BODS2C ( TARGET, TRGCDE, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'Could not translate body name #.' )
         CALL ERRCH  ( '#', TARGET                        )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'            )
         CALL CHKOUT ( 'T_TOPKER'                         )
         RETURN

      END IF

C
C     Create an SPK file containing data for the input sites.
C
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

         CALL SPKW08 ( HANDLE, SITIDS(I), TRGCDE, TRGREF, 
     .                 FIRST,  LAST,      SEGID,  DEGREE,
     .                 NSTATE, STATES,    FIRST,  STEP   )

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


C
C     Create frame kernel.
C
      IF ( EXISTS(FK) ) THEN

         CALL SETMSG ( 'Frame kernel # already exists; this '
     .   //            'program wont''t overwrite it.'       )
         CALL ERRCH  ( '#', FK                               )
         CALL SIGERR ( 'SPICE(FILEEXISTS)'                   )
         CALL CHKOUT ( 'T_TOPKER'                            )
         RETURN

      END IF

C
C     Open output file.
C
      CALL TXTOPN ( FK, UNIT )

C
C     Write ID word.
C
      CALL WRITLN ( IDWORD, UNIT )

      CALL WRITLN ( ' ',       UNIT )


      OUTLIN = '   FILE: '//FK(:RTRIM(FK))
      CALL WRITLN ( OUTLIN,    UNIT )
      CALL WRITLN ( ' ',       UNIT )

      
      OUTLIN = '   This file was created by T_TOPKER.'
      CALL WRITLN ( OUTLIN,    UNIT )     
      CALL WRITLN ( ' ',       UNIT )


C
C     Initialize data and text markers.
C
      BEGDAT =  CHAR(BSLASH) // 'begindata'
      BEGTXT =  CHAR(BSLASH) // 'begintext'

       

C
C     Create a body-name mapping for each central body, in case the
C     mapping is not already known to SPICE.
C
      CALL WRITLN ( ' ',    UNIT )

      OUTLIN = '   Body-name mapping follows:'
      CALL WRITLN ( OUTLIN, UNIT )

C
C     Start data area.
C
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

        
C
C     End data area.
C
      CALL WRITLN ( ' ',    UNIT )
      CALL WRITLN ( BEGTXT, UNIT )
      CALL WRITLN ( ' ',    UNIT )

C
C     Introduce the frame definitions.
C
      CALL WRITLN ( ' ',    UNIT )

      OUTLIN = '   Reference frame specifications follow:'
      CALL WRITLN ( OUTLIN, UNIT )
      CALL WRITLN ( ' ',    UNIT )
      CALL WRITLN ( ' ',    UNIT )

C
C     Start data area.
C
      CALL WRITLN ( ' ',    UNIT )
      CALL WRITLN ( BEGDAT, UNIT )
      CALL WRITLN ( ' ',    UNIT )


      DO I = 1, NSITES
C
C        Create the frame ID assignment.
C
         OUTLIN = ' '

         CALL LJUCRS ( 0, SITFNM(I), OUTLIN(LMARG:) )

         CALL PREFIX ( 'FRAME_', 0, OUTLIN(LMARG:) )

         OUTLIN(EQIDX:) = '='

         CALL INTSTR ( SITFID(I), OUTLIN(VALIDX:) )

         CALL WRITLN ( OUTLIN, UNIT )

C
C        Create the frame name assignment.
C
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

C
C        Create the frame class and class ID assignments.
C
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

C
C        Create the center assignment.
C
         OUTLIN          = ' '
         OUTLIN(LMARG:)  = FSTEM
         CALL SUFFIX ( '_CENTER',  0, OUTLIN(LMARG:) )

         OUTLIN(EQIDX:)  = '='

         CALL INTSTR ( SITIDS(I), OUTLIN(VALIDX:) )

         CALL WRITLN ( OUTLIN, UNIT )

C
C        Create the object-frame association assignment.
C
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

C
C        Create the relative frame assignment.
C
         CALL PREFIX ( 'TK', 0, FSTEM )
         
         OUTLIN         = ' '
         OUTLIN(LMARG:) = FSTEM
         CALL SUFFIX ( '_RELATIVE', 0, OUTLIN(LMARG:) )

         OUTLIN(EQIDX:)  = '='

         OUTLIN(VALIDX:) = TRGREF

         CALL PREFIX ( SQUOTE, 0, OUTLIN(VALIDX:) )
         CALL SUFFIX ( SQUOTE, 0, OUTLIN(VALIDX:) )

         CALL WRITLN ( OUTLIN, UNIT )


C
C        Create the frame and units assignments.
C
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

C
C        Create the axis sequence assignment.
C
         OUTLIN = ' '

         OUTLIN(LMARG:) = FSTEM

         CALL SUFFIX ( '_AXES', 0, OUTLIN(LMARG:) )

         OUTLIN(EQIDX:)  = '='

         OUTLIN(VALIDX:) = '( #, #, # )'

         DO J = 1, 3

            CALL REPMI ( OUTLIN(VALIDX:), '#', AXES(J,I), 
     .                   OUTLIN(VALIDX:)                  )

         END DO

         CALL WRITLN ( OUTLIN, UNIT )


C
C        Create the angle sequence assignments.
C
C
C        First convert the angles to strings. Scale to degrees
C        in the process.
C
         DO J = 1, 3

            ANGLE = ANGLES(J,I) * DPR()

            WRITE ( ANGSTR(J), FMT1, IOSTAT = IOSTAT )   ANGLE

            IF ( IOSTAT .NE. 0 ) THEN

               CALL SETMSG ( 'Attempt to write angle # for '
     .         //            'frame # failed; IOSTAT was #.'      )
               CALL ERRINT ( '#', J                               )
               CALL ERRCH  ( '#', SITFNM(I)                       )
               CALL ERRINT ( '#', IOSTAT                          )
               CALL SIGERR ( 'SPICE(WRITEFAILED)'                 )
               CALL CHKOUT ( 'T_TOPKER'                           )
               RETURN

            END IF

         END DO

C
C        First angle:
C
         OUTLIN = ' '
         OUTLIN(LMARG:) = FSTEM

         CALL SUFFIX ( '_ANGLES', 0, OUTLIN(LMARG:) )

         OUTLIN(EQIDX:)  = '=  ('

         OUTLIN(NUMIDX:) = ANGSTR(1)

         CALL SUFFIX ( ',', 0, OUTLIN(NUMIDX:) )

         CALL WRITLN ( OUTLIN, UNIT )

C
C        Second angle:
C
         OUTLIN = ' '

         OUTLIN(NUMIDX:) = ANGSTR(2)

         CALL SUFFIX ( ',', 0, OUTLIN(NUMIDX:) )

         CALL WRITLN ( OUTLIN, UNIT )

C
C        Third angle:
C
         OUTLIN = ' '

         OUTLIN(NUMIDX:) = ANGSTR(3)

         CALL SUFFIX ( ')', 1, OUTLIN(NUMIDX:) )

         CALL WRITLN ( OUTLIN, UNIT )
         
C
C        Add a little vertical white space between frame
C        specifications.
C
         CALL WRITLN ( ' ', UNIT )

      END DO
      
C
C     End data area.
C
      CALL WRITLN ( BEGTXT, UNIT )
      CALL WRITLN ( ' ',    UNIT )

      CALL WRITLN ( ' ',                          UNIT )
      CALL WRITLN ( '[End of definitions file]',  UNIT )
      CALL WRITLN ( ' ',                          UNIT )

C
C     Close output file.
C
      CLOSE ( UNIT )


      CALL CHKOUT ( 'T_TOPKER' )
      RETURN
      END


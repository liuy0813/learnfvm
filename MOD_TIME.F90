! TIME MODULE OF THE MODEL
! refered from pom and roms
module mod_time

  FUNCTION TIMEFUN(IY,IM,ID,IH,MIN,ISEC)
  IMPLICIT NONE
 !TIMEE=TIMEFUN(IYEARE,IMONTHE,IDAYE,IHOURE,IMINUTEE,0) ! IN HOURS
  REAL(kind=8) TIMEFUN
  INTEGER MN(12),MN1(12),IY,IM,ID,IH,MIN,Isec,iyy,iyy1
        INTEGER IDD
  DATA MN/0,31,59,90,120,151,181,212,243,273,304,334/
  DATA MN1/0,31,60,91,121,152,182,213,244,274,305,335/
  IDD=365*(IY-1)+(IY-1)/4-(IY-1)/100+(IY-1)/400
  IYY=(IY-1)/4-(IY-1)/100+(IY-1)/400
  IYY1=IY/4-IY/100+IY/400
  IF(IYY1.GT.IYY) THEN
    IDD=IDD+MN1(IM)+ID-1
  ELSE
    IDD=IDD+MN(IM)+ID-1
  ENDIF
  TIMEFUN=0.24D2*IDD+.1D1*IH+MIN/0.6D2+ISEC/3600.  ! IN HOURS
! print*,TIMEFUN,IY,IM,ID,IH,MIN
  RETURN
  END function

      SUBROUTINE get_date (date_str)
!
!svn $Id: get_date.F 751 2015-01-07 22:56:36Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2015 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!   This routine gets todays date, day of the week and time called     !
!  (default month & weekday are December & Saturday respectively).     !
!  It uses SUN intrinsic date routine by default.                      !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     date_str   Concatenated string for the day of the week, date     !
!                (month,day,year), and time (12hr clock) of day        !
!                (hour:min:sec).                                       !
!                                                                      !
!=======================================================================
!
      USE mod_kinds
!
      implicit none
!
!  Imported variable declarations.
!
      character (len=*), intent(out) :: date_str
!
!  Local variable declarations.
!
      integer :: iyear, imonth, iday, ihour, iminute, isecond
      integer :: Dindex, i, half, len1, len2, len3
      integer, dimension(8) :: values
      integer, dimension(31) :: lday =                                  &
     &          (/ (1,i=1,9), (2,i=1,22) /)
      integer, dimension(12) :: lmonth =                                &
     &          (/ 7, 8, 5, 5, 3, 4, 4, 6, 9, 7, 8, 8 /)
      character (len= 5) :: czone
      character (len= 8) :: cdate
      character (len=10) :: ctime
      character (len=11) :: tstring
      character (len=18) :: today
      character (len=20) :: fmt
      character (len=44) :: dstring
      character (len=3), dimension(0:1) :: ampm =                       &
     &                   (/' AM',' PM'/)
      character (len=9), dimension(0:6) :: day =                        &
     &                   (/ 'Sunday   ','Monday   ','Tuesday  ',        &
     &                      'Wednesday','Thursday ','Friday   ',        &
     &                      'Saturday ' /)
      character (len=9), dimension(12) :: month =                       &
     &                   (/ 'January  ','February ','March    ',        &
     &                      'April    ','May      ','June     ',        &
     &                      'July     ','August   ','September',        &
     &                      'October  ','November ','December ' /)
!
!-----------------------------------------------------------------------
!  Get weekday, date and time in short format, then extract its
!  information.
!-----------------------------------------------------------------------
!
      CALL date_and_time (cdate, ctime, czone, values)
!
      iyear=values(1)            ! 4-digit year
      imonth=values(2)           ! month of the year
      iday=values(3)             ! day of the month
      ihour=values(5)            ! hour of the day, local time
      iminute=values(6)          ! minutes of the hour, local time
      isecond=values(7)          ! seconds of the minute, local time
!
!-----------------------------------------------------------------------
!  Convert from 24 hour clock to 12 hour AM/PM clock.
!-----------------------------------------------------------------------
!
      half=ihour/12
      ihour=ihour-half*12
      IF (ihour.eq.0) ihour=12
      IF (half.eq.2) half=0
!
!-----------------------------------------------------------------------
!  Get index for the day of the week.
!-----------------------------------------------------------------------
!
      CALL day_code (imonth, iday, iyear, Dindex)
!
!-----------------------------------------------------------------------
!  Construct date, time and day of the week output string.
!-----------------------------------------------------------------------
!
      WRITE (fmt,10) lmonth(imonth), lday(iday)
 10   FORMAT ('(a',i1,',1x,i',i1,',1h,,1x,i4)')
      WRITE (today,fmt) month(imonth), iday, iyear
      dstring=day(Dindex)
      WRITE (tstring,20) ihour, iminute, isecond, ampm(half)
 20   FORMAT (i2,':',i2.2,':',i2.2,a3)
!
!  Concatenate date string.
!
      len1=LEN_TRIM(dstring)
      len2=LEN_TRIM(today)
      len3=LEN_TRIM(tstring)
      date_str=TRIM(ADJUSTL(dstring(1:len1)))
      IF (len2.gt.0) THEN
        len1=LEN_TRIM(date_str)
        WRITE (date_str,'(a," - ",a)') TRIM(date_str(1:len1)),          &
     &                                 TRIM(today(1:len2))
      END IF
      IF (len3.gt.0) THEN
        len1=LEN_TRIM(date_str)
        WRITE (date_str,'(a," - ",a)') TRIM(date_str(1:len1)),          &
     &                                 TRIM(tstring(1:len3))
      END IF
      RETURN
      END SUBROUTINE get_date

      SUBROUTINE time_string (time, time_code)
!
!=======================================================================
!                                                                      !
!  This routine encodes current model time in seconds to a time        !
!  string of the form:                                                 !
!                                                                      !
!       DDDDD HH:MM:SS                                                 !
!                                                                      !
!  This allow a more accurate label when reporting time.               !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     time       Current model time  (seconds)                         !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     time_code  Current model time string.                            !
!                                                                      !
!=======================================================================
!
      USE mod_kinds
!
      implicit none
!
!  Imported variable declarations.
!
      real(r8), intent(in) :: time
      character(len=14), intent(out) :: time_code
!
!  Local variable declarations.
!
      integer :: iday, ihour, isec, minute
      character (len=14) :: text
!
!-----------------------------------------------------------------------
!  Encode current mode time.
!-----------------------------------------------------------------------
!
      iday=INT(time/86400)
      isec=INT(time-iday*86400.0_r8)
      ihour=isec/3600
      minute=MOD(isec,3600)/60
      isec=MOD(isec,60)
!
      WRITE (text,10) iday, ihour, minute, isec
 10   FORMAT (i5,1x,i2.2,':',i2.2,':',i2.2)
      time_code=text
      RETURN
      END SUBROUTINE time_string

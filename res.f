      subroutine res
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes water and sediment through reservoirs
!!    computes evaporation and seepage from the reservoir.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    br1(:)       |none          |1st shape parameter for reservoir surface
!!                                |area equation
!!    br2(:)       |none          |2nd shape parameter for reservoir surface
!!                                |area equation
!!    curyr        |none          |current year of simulation
!!    evrsv(:)     |none          |lake evaporation coefficient
!!    iflod1r(:)   |none          |beginning month of non-flood season
!!                                |(needed if IRESCO=2)
!!    iflod2r(:)   |none          |ending month of non-flood season
!!                                |(needed if IRESCO=2)
!!    inum1        |none          |reservoir number
!!    iresco(:)    |none          |outflow simulation code:
!!                                |0 compute outflow for uncontrolled reservoir
!!                                |  with average annual release rate
!!                                |1 measured monthly outflow
!!                                |2 simulated controlled outflow-target release
!!                                |3 measured daily outflow
!!                                |4 stage/volume/outflow relationship 
!!    i_mo         |none          |current month of simulation
!!    ndtargr(:)   |days          |number of days to reach target storage from
!!                                |current reservoir storage
!!                                |(needed if IRESCO=2)
!!    oflowmn(:,:) |m^3/day       |minimum daily ouflow for the month
!!    oflowmx(:,:) |m^3/day       |maximum daily ouflow for the month
!!    pet_day      |mm H2O        |potential evapotranspiration on day
!!    res_evol(:)  |m**3          |volume of water needed to fill the reservoir
!!                                |to the emergency spillway
!!    res_k(:)     |mm/hr         |hydraulic conductivity of the reservoir 
!!                                |bottom
!!    res_nsed(:)  |kg/L          |normal amount of sediment in reservoir
!!    res_pvol(:)  |m**3          |volume of water needed to fill the reservoir
!!                                |to the principal spillway 
!!    res_rr(:)    |m**3/day      |average daily principal spillway release
!!                                |volume
!!    res_sed(:)   |kg/L (ton/m^3)|amount of sediment in reservoir
!!    res_sub(:)   |none          |number of subbasin reservoir is in
!!    res_vol(:)   |m^3 H2O       |reservoir volume
!!    resflwi      |m^3 H2O       |water entering reservoir on day
!!    res_out(:,:,:)|m**3/day      |measured average daily outflow from the
!!                                |reservoir for the month
!!    ressedi      |metric tons   |sediment entering reservoir during time step
!!    sub_subp(:)  |mm H2O        |precipitation for day in subbasin
!!    sub_sumfc(:) |mm H2O        |amount of water in subbasin soil at field 
!!                                |capacity
!!    sub_sw(:)    |mm H2O        |amount of water in soil profile in subbasin
!!    starg(:,:)   |m**3          |monthly target reservoir storage
!!    wuresn(:,:)  |m**3          |average amount of water withdrawn from
!!                                |reservoir each month for consumptive water 
!!                                |use
!!    wurtnf(:)    |none          |fraction of water removed from the reservoir
!!                                |via WURESN which is returned and becomes flow
!!                                |from the reservoir outlet
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    res_sed(:)  |kg/L (ton/m^3)|amount of sediment in reservoir
!!    res_vol(:)  |m^3 H2O       |reservoir volume
!!    resev       |m^3 H2O       |evaporation from reservoir on day
!!    resflwo     |m^3 H2O       |water leaving reservoir on day
!!    respcp      |m^3 H2O       |precipitation on reservoir for day
!!    ressa       |ha            |surface area of reservoir on day
!!    ressep      |m^3 H2O       |seepage from reservoir on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    flw         |m^3/s         |reservoir outflow for day
!!    jres        |none          |reservoir number
!!    sed         |kg/L          |concentration of sediment in reservoir at
!!                               |beginning of day
!!    targ        |m^3 H2O       |target reservoir volume for day
!!    vol         |m^3 H2O       |volume of water stored in reservoir at 
!!                               |beginning of day
!!    vvr         |m^3 H2O       |maximum controlled water release for day
!!    xx          |none          |variable to hold intermediate calculation 
!!                               |result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: jres
      real :: vol, sed, vvr, targ, xx, flw
	real :: san,sil,cla,sag,lag,gra,ndespill
	real :: inised, finsed, setsed, remsetsed
 
      jres = 0
      jres = inum1

!! store initial values
      vol = 0.
      sed = 0.
	inised = 0.
	finsed = 0.
	setsed = 0.
	remsetsed = 0.


        !! determine reservoir outflow
        select case (iresco(jres))
          case (3)                   !! use measured daily outflow
            flw = 0.
            read (350+jres,*) flw
            resflwo = 86400. * flw + q26
        end select
          

      return
 5000 format (f8.2)
      end
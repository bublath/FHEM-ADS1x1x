##############################################
# $Id: 52_I2C_ADS1x1x.pm$
# Credits:
# Texas Instruments for the Chip - Documentation at https://www.ti.com/lit/ds/sbas444d/sbas444d.pdf (ADS111x) 
#                                               and https://www.ti.com/lit/ds/sbas473e/sbas473e.pdf (ADS101x)
# Karsten Grüttner - for the initial ADS1x1x implementation
# Klaus Wittstock: for the PCF8574 module that I used as a basis for this revised ADS1x1x implementation
#
#
package main;

use strict;
use warnings;
use SetExtensions;
use Scalar::Util qw(looks_like_number);

my %setsP = (
'off' => 0,
'on' => 1,
);

my %I2C_ADS1x1x_Config =
(

	'State' => #	Bit [15]
		{
			'SINGLE'  => 	1 << 15,  	# Write: Begin a single conversion (when in power-down mode)
			'BUSY'    =>  	0,  		# Read: Bit = 0 Device is currently performing a conversion
			'NOT_BUSY' => 	1 << 15   	# Read: Bit = 1 Device is not currently performing a conversion	
		},		
	'Mux' =>  #	 Bits [14:12]
		{
			'COMP_0_1' => 0 , 		# AINP = AIN0 and AINN = AIN1 , default
			'COMP_0_3' => 1 << 12,	# AINP = AIN0 and AINN = AIN3
			'COMP_1_3' => 2 << 12 , # AINP = AIN1 and AINN = AIN3
			'COMP_2_3' => 3 << 12 , # AINP = AIN2 and AINN = AIN3
			'SINGLE_0' => 4 << 12 , # AINP = AIN0 and AINN = GND
			'SINGLE_1' => 5 << 12 , # AINP = AIN1 and AINN = GND
			'SINGLE_2' => 6 << 12 , # AINP = AIN2 and AINN = GND 
			'SINGLE_3' => 7 << 12 	# AINP = AIN3 and AINN = GND	
		},
	'Gain' => # Bits [11:9]
		{
		    '6V' => 	{ code => 0, 		refVoltage => 6.144 },
			'4V' => 		{ code => 1 << 9, 	refVoltage => 4.096 },   # default
			'2V' => 		{ code => 2 << 9, 	refVoltage => 2.048 },
			'1V' => 		{ code => 3 << 9, 	refVoltage => 1.024 },
			'0.5V' => 		{ code => 4 << 9, 	refVoltage => 0.512 },
			'0.25V' => 	{ code => 5 << 9,  	refVoltage => 0.256 }
			
		},
	'Data_Rate' => # Bits [7:5] "delay" refers to ADS111x only, but not used at all currently
		{
			'1/16x'   	=> { code => 0,			delay => 1.0/8   },
			'1/8x'    	=> { code => 1 << 5,	delay => 1.0/16  },
			'1/4x'    	=> { code => 2 << 5,	delay => 1.0/32  },
			'1/2x'    	=> { code => 3 << 5,	delay => 1.0/64  },
			'1x'   	=> { code => 4 << 5,	delay => 1.0/128 }, # default
			'2x'   	=> { code => 5 << 5,	delay => 1.0/250 },
			'4x'   	=> { code => 6 << 5,	delay => 1.0/475 },
			'8x'	=> { code => 7 << 5,	delay => 1.0/860 }
		},		
	'Operation_Mode' => # Bit [8]
		{
			'Continuously' 	=> 0, 		# einmalig initialisiert, kann immer gelesen werden, geeignet für Dauerüberwachung 
			'SingleShot'	=> 1 << 8 	# wacht zum einmaligen Lesen auf und legt sich wieder schlafen, geeignet für Messungen mit großen Pausen dazwischen
		},

	'Comparator_Mode' =>	# Bit [4]
		{
			'Traditional' => 	0,
			'Window' =>			1 << 4
		},
	'Comparator_Polarity' =>	# Bit [3]
		{
			'ActiveLow' => 0,  		# default
			'ActiveHigh' => 1 << 3	
			
		},
	'Latching_Comparator' =>	# Bit [2]
		{
			'off' => 	0,			, # default	
			'on' => 	1 << 2
		},
	'Comparator_Queue_Disable' => # Bits [1:0]
		{
			'AfterOneConversion' 	=> 0, 
			'AfterTwoConversions' 	=> 1, 
			'AfterFourConversions' 	=> 2,
			'disable'				=> 3 	# default	
		}
		
);

sub I2C_ADS1x1x_Initialize($) {
  my ($hash) = @_;

  $hash->{DefFn}     = 	"I2C_ADS1x1x_Define";
  $hash->{InitFn}  	 =  'I2C_ADS1x1x_Init';
  $hash->{AttrFn}    = 	"I2C_ADS1x1x_Attr";
  $hash->{SetFn}     = 	"I2C_ADS1x1x_Set";
  $hash->{StateFn}   =  "I2C_ADS1x1x_State";
  $hash->{GetFn}     = 	"I2C_ADS1x1x_Get";
  $hash->{UndefFn}   = 	"I2C_ADS1x1x_Undef";
  $hash->{I2CRecFn}  = 	"I2C_ADS1x1x_I2CRec";
  $hash->{AttrList}  = 	"IODev do_not_notify:1,0 ignore:1,0 showtime:1,0 ".
												"a0_mode:RTD,NTC,RAW,RES,off ".
												"a1_mode:RTD,NTC,RAW,RES,off ".
												"a2_mode:RTD,NTC,RAW,RES,off ".
												"a3_mode:RTD,NTC,RAW,RES,off ".
												"a0_res a1_res a2_res a3_res ".
												"a0_r0 a1_r0 a2_r0 a3_r0 ".
												"a0_bval a1_bval a2_bval a3_bval ".												
												"a0_gain:6V,4V,2V,1V,0.5V,0.25V ".
												"a1_gain:6V,4V,2V,1V,0.5V,0.25V ".
												"a2_gain:6V,4V,2V,1V,0.5V,0.25V ".
												"a3_gain:6V,4V,2V,1V,0.5V,0.25V ".
												"decimals:0,1,2,3,4,5 ".
												"sys_voltage " .
												"data_rate:1/16x,1/8x,1/4x,1/2x,1x,2x,4x,8x ".
												"mux:SINGLE ".
												"device:ADS1013,ADS1014,ADS1015,ADS1113,ADS1114,ADS1115 ".
												#"comparator_polarity:ActiveLow,ActiveHigh ".
												#"operation_mode:SingleShot,Continuously ".  ### Does not make sense with multiple inputs
												#"comparator_mode:Traditional,Window ".
												#"latching_comparator:on,off ".
												#"comparator_queue_disable:AfterOneConversion,AfterTwoConversion,AfterFourConversion,disable ".
												"poll_interval ".
												"poll_interleave ".
												"$readingFnAttributes";
}
################################### Todo: Set or Attribute for Mode? Other sets needed?
sub I2C_ADS1x1x_Set($@) {					#
	my ($hash, @a) = @_;
	my $name =$a[0];
	my $cmd = $a[1];
	my $val = $a[2];	
 
	if ( $cmd && $cmd eq "Update") {
		#Make sure there is no reading cycle running and re-start polling (which starts with an inital read)
		RemoveInternalTimer($hash) if ( defined (AttrVal($hash->{NAME}, "poll_interval", undef)) ); 
		#readingsSingleUpdate($hash, 'state', 'Polling',0);
		I2C_ADS1x1x_Poll($hash);
		return undef;
	} elsif ($cmd && $cmd eq "Reopen") {
		I2C_ADS1x1x_Initialize($hash);
		return undef;
	} else {
		my $list = "Update:noArg Reopen:noArg";
		return "Unknown argument $a[1], choose one of " . $list if defined $list;
		return "Unknown argument $a[1]";
	}
	return "$name: no IO device defined" unless ($hash->{IODev});
  	return undef;
}
################################### 
sub I2C_ADS1x1x_Get($@) {
	my ($hash) = @_;
	Log3 $hash->{NAME}, 3, $hash->{NAME}." => Get";
	$hash->{helper}{state}=0; #Reset states just in case
	#InternalTimer(gettimeofday()+1, \&I2C_ADS1x1x_Execute, $hash,0);
	return undef;
}

sub I2C_ADS1x1x_Execute($@) {
	my ($hash) = @_;
	my $state=$hash->{helper}{state};
	my $device=AttrVal($hash->{NAME}, "device", "ADS1115");
	my $channels=1;
	#Default time between reading channels
	my $nexttimer=AttrVal($hash->{NAME}, 'poll_interleave', 0.008);
	if (!defined($state)) {$state=0};
	if ($state%2) {$nexttimer=0.008;} #8 ms conversiontime for even numbers
	if ($device =~ m/^ADS1[0|1]15$/i ) {$channels=4;} # Only these two devices have 4 channels
	if ($state<$channels*2) {
		$hash->{helper}{state}+=1;	
	} else {
		$hash->{helper}{state}=0;
		#Interleave to next complete read cycle is poll interval
		$nexttimer = AttrVal($hash->{NAME}, 'poll_interval', 5)*60;
	}
	Log3 $hash->{NAME}, 3, $hash->{NAME}." => Processing state $state timer $nexttimer channels: $channels newstate:".$hash->{helper}{state};
	if (!defined AttrVal($hash->{NAME}, "IODev", undef)) {return;}
	if ($state==0) {
		I2C_ADS1x1x_InitConfig($hash,0);
	} elsif ($state==1) {
		I2C_ADS1x1x_ReadData($hash,0);
	} elsif ($state==2) {
		I2C_ADS1x1x_InitConfig($hash,1);
	} elsif ($state==3) {
		I2C_ADS1x1x_ReadData($hash,1);
	} elsif ($state==4) {
		I2C_ADS1x1x_InitConfig($hash,2);
	} elsif ($state==5) {
		I2C_ADS1x1x_ReadData($hash,2);
	} elsif ($state==6) {
		I2C_ADS1x1x_InitConfig($hash,3);
	} elsif ($state==7) {
		I2C_ADS1x1x_ReadData($hash,3);
	}
	
	#Initalize next Timer for Reading Results in 8ms (time required for conversion to be ready)
	InternalTimer(gettimeofday()+$nexttimer, \&I2C_ADS1x1x_Execute, $hash,0);
	return undef;
}

sub I2C_ADS1x1x_InitConfig(@) {
	my ($hash, $sensor) = @_;
	my $phash = $hash->{IODev};
	my $pname = $phash->{NAME};
	my $mux=AttrVal($hash->{NAME}, "mux", "SINGLE");
	my $device=AttrVal($hash->{NAME}, "device", "ADS1115");
	my $rate=AttrVal($hash->{NAME}, "data_rate", "1x");
	my $opmode=AttrVal($hash->{NAME}, "operation_mode", "SingleShot");
	my $cmode=AttrVal($hash->{NAME}, "comparator_mode", "Traditional");
	my $lcomp=AttrVal($hash->{NAME}, "latching_comparator", "on");
	my $cqueue=AttrVal($hash->{NAME}, "comparator_queue_disable", "AfterOneConversion");
	my $cpol=AttrVal($hash->{NAME}, "comparator_polarity", "ActiveLow");
	return undef if ($mux ne "SINGLE"); #Only SINGLE mode supported
	my $gain=AttrVal($hash->{NAME}, "a".$sensor."_gain", "4V"); 
	my $mode=AttrVal($hash->{NAME}, "a".$sensor."_mode", "RAW");
	my $sensval=$mux."_".$sensor;
	if ($mode ne "off") {
		Log3 $hash->{NAME}, 5, $hash->{NAME}." => $pname Config:".$sensval." ".$rate." ".$opmode." ".$cmode." ".$lcomp." ".$cqueue." ".$cpol;	
		my $config = $I2C_ADS1x1x_Config{'State'}{SINGLE}|
			$I2C_ADS1x1x_Config{'Mux'}{$sensval}|
			$I2C_ADS1x1x_Config{'Data_Rate'}{$rate}{code}|
			$I2C_ADS1x1x_Config{'Gain'}{$gain}{code}|
			$I2C_ADS1x1x_Config{'Operation_Mode'}{$opmode}|
			$I2C_ADS1x1x_Config{'Comparator_Mode'}{$cmode}|
			$I2C_ADS1x1x_Config{'Latching_Comparator'}{$lcomp}|
			$I2C_ADS1x1x_Config{'Comparator_Queue_Disable'}{$cqueue}|				
			$I2C_ADS1x1x_Config{'Comparator_Polarity'}{$cpol};
				
		my $low_byte = $config & 0xff;
		my $high_byte = ($config & 0xff00) >> 8;	
		my %sendpackage = ( i2caddress => $hash->{I2C_Address}, direction => "i2cwrite", reg=> 1, sensor=>$sensor, data => $high_byte. " " .$low_byte);
		Log3 $hash->{NAME}, 4, $hash->{NAME}." => $pname CONFIG adr:".$hash->{I2C_Address}." Sensor $sensor Byte0:$high_byte Byte1:$low_byte";
	    CallFn($pname, "I2CWrtFn", $phash, \%sendpackage);
	}
}

sub I2C_ADS1x1x_ReadData(@) {
	my ($hash, $sensor) = @_;
	my $phash = $hash->{IODev};
	my $pname = $phash->{NAME};
	#Gain needs to be passed through for calculation
	my $gain=AttrVal($hash->{NAME}, "a".$sensor."_gain", "4V"); 
	my %sendpackage = ( i2caddress => $hash->{I2C_Address}, direction => "i2cread", reg=> 0, sensor=>$sensor, gain=>$gain, nbyte => 2);
	Log3 $hash->{NAME}, 4, $hash->{NAME}." => $pname READ adr:".$hash->{I2C_Address}." Sensor $sensor Gain $gain";
	CallFn($pname, "I2CWrtFn", $phash, \%sendpackage);
}

################################### 
sub I2C_ADS1x1x_Attr(@) {					#
 my ($command, $name, $attr, $val) = @_;
 my $hash = $defs{$name};
 my $msg = undef;
  if ($command && $command eq "set" && $attr && $attr eq "IODev") {
		if ($main::init_done and (!defined ($hash->{IODev}) or $hash->{IODev}->{NAME} ne $val)) {
			main::AssignIoPort($hash,$val);
			my @def = split (' ',$hash->{DEF});
			I2C_ADS1x1x_Init($hash,\@def) if (defined ($hash->{IODev}));
		}
	}
  if ($attr eq 'poll_interval') {
    if ( defined($val) ) {
      if ( looks_like_number($val) && $val > 0) {
        RemoveInternalTimer($hash);
        InternalTimer(1, 'I2C_ADS1x1x_Execute', $hash, 0);
      } else {
        $msg = "$hash->{NAME}: Wrong poll intervall defined. poll_interval must be a number > 0";
      }    
    } else {
      RemoveInternalTimer($hash);
    }
	return $msg;
  }
  #check for correct values while setting so we need no error handling later
  foreach ('sys_voltage','a0_res','a1_res','a2_res','a3_res', 'a0_r0', 'a1_r0', 'a2_r0', 'a3_r0', 'a0_bval', 'a1_bval', 'a2_bval', 'a3_bval') {
	if ($attr eq $_) {
		if ( defined($val) ) {
			if ( !looks_like_number($val) || $val <= 0) {
				$msg = "$hash->{NAME}: ".$attr." must be a number > 0";
			}
		}
	}
  }
  return $msg;	
}
################################### 
sub I2C_ADS1x1x_Define($$) {			#
 my ($hash, $def) = @_;
 Log3 $hash->{NAME}, 3, $hash->{NAME}." => Define";
 my @a = split("[ \t]+", $def);
 readingsSingleUpdate($hash, 'state', 'Defined',0);
 if ($main::init_done) {
    eval { I2C_ADS1x1x_Init( $hash, [ @a[ 2 .. scalar(@a) - 1 ] ] ); };
    return I2C_ADS1x1x_Catch($@) if $@;
  }
  return undef;
}
################################### 
sub I2C_ADS1x1x_Init($$) {				#
	my ( $hash, $args ) = @_;
	#my @a = split("[ \t]+", $args);
	my $name = $hash->{NAME};
	Log3 $hash->{NAME}, 3, $hash->{NAME}." => Init";
	$hash->{helper}{state}=0; #initalize state machine
	if (defined $args && int(@$args) != 1)	{
		return "Define: Wrong syntax. Usage:\n" .
		       "define <name> I2C_ADS1x1x <i2caddress>";
	}
	if (defined (my $address = shift @$args)) {
		$hash->{I2C_Address} = $address =~ /^0.*$/ ? oct($address) : $address; 
	} else {
 		return "$name I2C Address not valid";
	}
  	AssignIoPort($hash);
	readingsSingleUpdate($hash, 'state', 'Initialized',0);
	I2C_ADS1x1x_Set($hash, $name, "setfromreading");
	my $pollInterval = AttrVal($hash->{NAME}, 'poll_interval', 0);
	Log3 $hash->{NAME}, 3, $hash->{NAME}." => Init: Timer interval $pollInterval";
	InternalTimer(gettimeofday() + ($pollInterval * 60), 'I2C_ADS1x1x_Poll', $hash, 0) if ($pollInterval > 0);
	return;
}

################################### 
sub I2C_ADS1x1x_Catch($) {
	my $exception = shift;
	if ($exception) {
		$exception =~ /^(.*)( at.*FHEM.*)$/;
		return $1;
	}
	return undef;
}
################################### 
sub I2C_ADS1x1x_State($$$$) {			#reload readings at FHEM start
	my ($hash, $tim, $sname, $sval) = @_;
	#No persistant data needed, using only attributes
	return undef;
}
################################### 
sub I2C_ADS1x1x_Undef($$) {				#
	my ($hash, $name) = @_;
	RemoveInternalTimer($hash) if ( defined (AttrVal($hash->{NAME}, "poll_interval", undef)) ); 
	return undef;
}

# Calculate temperature for PT1000/PT100 platinum temperature sensors
# ax_r0 = Resistance in Ohm at zero degrees C
sub I2C_ADS1x1x_RTD($@) {
	my ($resistance,$sensor,$r0) = @_;
    #my $aa=0.003851; #Deutscher Standard? 
    my $aa=0.0039083; #ITU-90 Standard 
    my $bb=-5.05E-08; #my own value
    #my $bb=-5.7750E-07; #ITU-90 Standard
	my $temperature=0;
	my $root = $aa*$aa*$r0*$r0-4*$bb*$r0*($r0-$resistance);
	if ($root>=0) {
		$temperature=(-$aa*$r0+sqrt($root))/(2*$bb*$r0);
	}
	return $temperature;
}

# Calculate temperature for NTC Sensors 
# ax_r0 = Resistance in Ohm at 25 degrees C (typically 50K)
# ax_b = B-Value according to datasheet (for 50K often 3950)
sub I2C_ADS1x1x_NTC($@) {
	my ($resistance,$sensor,$r0,$bval) = @_;
	if ($resistance<0) {return 0;} # Prevent issue in error case
	my $steinhart;
	$steinhart = $resistance / $r0;    # (R/Ro)
	$steinhart = log($steinhart); # ln(R/Ro)
	$steinhart = $steinhart/$bval;                 # 1/B * ln(R/Ro)
	$steinhart = $steinhart+ 1.0 / (25.0 + 273.15);  # + (1/To)
	$steinhart = 1.0 / $steinhart;       # Invert
	$steinhart = $steinhart-273.15;               # convert to C
	return $steinhart;
}

################################### 

sub I2C_ADS1x1x_I2CRec($@) {				# ueber CallFn vom physical aufgerufen
	my ($hash, $clientmsg) = @_;
	my $name = $hash->{NAME};
	my $phash = $hash->{IODev};
	my $pname = $phash->{NAME};
	my $clientHash = $defs{$name};
	my $msg = "";
	while ( my ( $k, $v ) = each %$clientmsg ) { 	#erzeugen von Internals fuer alle Keys in $clientmsg die mit dem physical Namen beginnen
		$hash->{$k} = $v if $k =~ /^$pname/ ;
		$msg = $msg . " $k=$v";
	} 
	Log3 $hash,5 , "$name: I2C reply:$msg";
	my $sval;	
	if ($clientmsg->{direction} && $clientmsg->{$pname . "_SENDSTAT"} && $clientmsg->{$pname . "_SENDSTAT"} eq "Ok") {
		readingsBeginUpdate($hash);
		if ($clientmsg->{direction} eq "i2cread" && defined($clientmsg->{received})) {
			my ($high,$low) = split(/ /, $clientmsg->{received});
			my $value= $high<<8|$low;
			Log3 $hash,5 , "$name:value:$value";
			my $gain=$clientmsg->{gain};
			my $refvoltage=$I2C_ADS1x1x_Config{'Gain'}{$gain}{refVoltage};

			my $device=AttrVal($hash->{NAME}, "device", "ADS1115");
			my $mask=0x7fff;
			my $bits=16;
			#No differentiation for 12bit since those devices still submit 16bits with the 4 lower bits set to zero
			my $voltage = ($value & $mask) * 						# filtere Bit 2^15 (0x8000) raus, das ist Vorzeichenmerkmal
			( $refvoltage/$mask) * 									# normiere anhand der Auflösung 2^15 im positiven Bereich
			( 1.0 - (2.0 *  (($value & ($mask+1)) >> ($bits-1)))); 		# bei gesetzten Bit 2^15 Faktor -1, ansonsten +1	($mask+1 = 0x8000/0x800)

			#rounded voltage only for reading, continue calculation will full precision
			my $voltager = sprintf( '%.' . AttrVal($clientHash->{NAME}, 'decimals', 3) . 'f', $voltage 	); 
			Log3 $hash,5 , "$name:voltage=$voltage, ref=".$I2C_ADS1x1x_Config{'Gain'}{$gain}{refVoltage};
			my $sensor= $clientmsg->{sensor};
     		readingsBulkUpdate($hash, "a".$sensor."_voltage", $voltager) if (ReadingsVal($name,"a".$sensor."_voltage",0) != $voltager);
			my $divider=AttrVal($name,"a".$sensor."_res",1000);
			my $highvoltage=AttrVal($name,"sys_voltage",3.3);
			#Always calculate resistance but only write to reading in case of "RES" mode 
			my $resistance=$divider*$voltage/($highvoltage-$voltage);
			my $resistancer = sprintf( '%.' . AttrVal($name, 'decimals', 3) . 'f', $resistance 	);	
			my $temperature=0;
			Log3 $hash,5 , "$name:resistance=$resistance, with divider=$divider system_voltage=$highvoltage";
			if (AttrVal($name,"a".$sensor."_mode",0) eq "RES") {
				readingsBulkUpdate($hash, "a".$sensor."_resistance", $resistancer) if (ReadingsVal($name,"a".$sensor."_resistance",0) != $resistancer);
			}
			if (AttrVal($name,"a".$sensor."_mode",0) eq "RTD") {
				$temperature=sprintf( '%.1f', I2C_ADS1x1x_RTD($resistance,$sensor,AttrVal($name,"a".$sensor."_r0",1000.0)));
				Log3 $hash,5 , "$name:RTD Temp=$temperature °C";
				readingsBulkUpdate($hash, "a".$sensor."_temperature", $temperature) if (ReadingsVal($name,"a".$sensor."_temperature",0) != $temperature);
			}
			if (AttrVal($name,"a".$sensor."_mode",0) eq "NTC") {
				$temperature=sprintf( '%.1f', I2C_ADS1x1x_NTC($resistance,$sensor,AttrVal($name,"a".$sensor."_res",50000.0),AttrVal($name,"a".$sensor."_b",3950.0)));
				Log3 $hash,5 , "$name:NTC Temp=$temperature °C";
				readingsBulkUpdate($hash, "a".$sensor."_temperature", $temperature) if (ReadingsVal($name,"a".$sensor."_temperature",0) != $temperature);
			}
		} elsif ($clientmsg->{direction} eq "i2cwrite" && defined($clientmsg->{data})) {
			#reply from write - ignore
		}
    	readingsEndUpdate($hash, 1);
	}
}

1;

#Todo Write update documentation

=pod
=item device
=item summary controls/reads GPIOs from an via I2C connected ADS1x1x port extender
=item summary_DE steuern/lesen der GPIOs eines &uuml;ber I2C angeschlossenen ADS1x1x
=begin html

<a name="I2C_ADS1x1x"></a>
<h3>I2C_ADS1x1x</h3>
(en | <a href="commandref_DE.html#I2C_ADS1x1x">de</a>)
<ul>
	<a name="I2C_ADS1x1x"></a>
		Provides an interface to an ADS1x1x A/D converter via I2C.<br>
		The I2C messages are send through an I2C interface module like <a href="#RPII2C">RPII2C</a>, <a href="#FRM">FRM</a>
		or <a href="#NetzerI2C">NetzerI2C</a> so this device must be defined first.<br><br>
		<b>Limitations:</b><br><br>
		For simplification most settings can only be set for all 4 channels globally.<br>
		Comparator Mode (delta between two channels) is not supported.<br><br>
		<b>Special features:</b><br>
		Device supports reading voltages (RAW), resistance (RES) with divider resistor and temperature measurements of RTD (Platin Resistors like PT1000 or PT100) and NTC.
		<br>
		<b>Circuit:</b>
		To measure resistance and temperature (thermistors) your circuit should look like this:
		<br>
		<code>
		(T)----GND<br>
		  |<br>
		  |-----(A0)-----(R0)-----VCC<br>
		 <br>
		 T= Temperature Sensor or Resistor<br>
		 R0= Pull-up Resistor (typically in the same range as the resistance you measure (e.g 1KOhm for PT1000)<br>
		 A0= Connected to A0 Port of ADS1x1x (same for A1,A2,A3)<br>
		</code>
		<br>
		<br><b>Attribute IODev must be set.</b><br>         
	<a name="I2C_ADS1x1xDefine"></a><br>
	<b>Define</b>
	<ul>
		<code>define &lt;name&gt; I2C_ADS1x1x &lt;I2C Address&gt;</code><br>
		where <code>&lt;I2C Address&gt;</code> is without direction bit<br>
		<br>
	</ul>

	<a name="I2C_ADS1x1xSet"></a>
	<b>Set</b>
	<ul>
		<code>set &lt;name&gt; &lt;Update&gt;</code><br><br>
			<ul>
			<li>Trigger a reading<br>
			</ul>
		<br>
	</ul>

	<a name="I2C_ADS1x1xAttr"></a>
	<b>Attributes</b>
	<ul>
		<li>device<br>
			Defines the Texas Instruments ADS1x1x device that is actually being used.
			<br>
			<li>ADS1013 - 12Bit, 1 channel<br>
			<li>ADS1014 - 12Bit, 1 channel with Comparator<br>
			<li>ADS1015 - 12Bit, 4 channels with Comparator<br>
			<li>ADS1113 - 16Bit, 1 channel<br>
			<li>ADS1114 - 16Bit, 1 channel with Comparator<br>
			<li>ADS1115 - 16Bit, 4 channels with Comparator<br>
			<br>
			Note that the comparator feature is not supported by this module (so no difference between ADSxx13 and ADSxx14 is made).<br>
			Default: ADS1115<br>
		</li>
		<li>poll_interval<br>
			Set the polling interval in minutes to query a new reading from enabled channels<br>
			Default: -, valid values: decimal number<br>
		</li>
		<br>
		<li>sys_voltage<br>
			System voltage running the chip and typically connected to the pull-up resistor (e.g. 3.3V with a Raspberry Pi)<br>
			Default: 3.3, valid values: float number<br>
		</li>
		<br>
		<li>decimals<br>
			Number of decimals (after the decimal point) for voltage and resistance to make results more readable. Calculations are still based on full precision. Temperatures are fixed to one decimal.<br>
			Default: 3, valid values: 0,1,2,3,4,5<br>
		</li>
		<br>
		<li>a[0-3]_gain<br>
			Gain amplifier value (sensibility and range of measurement) used per channel a0-a3. Standard is 4V which can measure a range between 0 and 4 Volts. If measuring smaller voltage, the amplification can be increased to get more accurate readings. The module will automatically calculate the value back to the correct voltage output.<br>
			Default: 4V, valid values: 6V,4V,2V,1V,0.5V,0.25V<br>
		</li>
		<br>
		<li>a[0-3]_mode<br>
			Determines how the results are interpreted.
			<ul>
			<li>off: The channel is not measured<br>
			<li>RAW: Only voltage is measured and placed in reading a[0-3]_voltage<br>
			<li>RES: Plain resistor measurement, typically needs a pull-up resistor defined by a[0-3]_res. Reading in a[0-3]_resistance<br>
			<li>RTD: For Platin temperature resistors (PT1000,PT100), like RES needs a pull-up and also reference resistance at 0°C in a[0-3]_r0. Reading in a[0-3]_temperature<br>
			<li>NTC: For NTC Thermistors, like RES needs a pull-up, reference resistance at 25°C in a[0-3]_r0 and B-value in a[0-3]_b. Reading in a[0-3]_temperature<br>
			</ul>
		</li>
		<br>		
		<li>a[0-3]_res<br>
			Value of pull-up resistor for resistance and temperature measurement. Connected between A0 and VCC (defined in "sys_voltage")<br>
			Default: 1000, valid values: float numbers<br>
		</li>
		<br>		
		<li>a[0-3]_r0<br>
			Reference resistance for temperature measurements at 0°C (for RTD) and 25°C (for NTC) in Ohm.<br>
			Default: 1000.0 in RTD and 50000.0 in NTC mode, valid values: float numbers<br>
		</li>
		<br>		
		<li>a[0-3]_b<br>
			B-Value for NTC Thermistors (define the increase from the base value).<br>
			Default: 3950.0, valid values: float numbers<br>
		</li>
		<br>		
		<li>operation_mode<br>
			<ul>
			Not implemented, since Continuous Mode make no sense when using multiple input registers and is meant to read values in very high speed (e.g. one value every 8 ms) which IMHO makes no sense with FHEM.<br> 
			<li>SingleShot: Do one reading and then power down<br>
			<li>Continuously: Keep powered on and continiously read data<br>
			</ul>
		</li>
		<br>
		<li>data_rate (1/16x,1/8x,1/4x,1/2x,1x,2x,4x,8x )<br>
			<ul>
			Conversion speed - default is 1x. The 12-bit chips use 1600 SPS as default rate, while the 16-bit chips are slower with 128 SPS. 
			Below table translates the settings based on the actual device used.<br>
			<table border = "1">
			<tr>
				<td>Data Rate</td>
				<td>ADS101x Setting</td>
				<td>ADS111x Settings</td>
			</tr>
            <tr>
				<td>1/16x</td>
				<td>128_SPS</td>
				<td>8_SPS</td>
			</tr>
            <tr>
				<td>1/8x</td>
				<td>250_SPS</td>
				<td>16_SPS</td>
			</tr>
            <tr>
				<td>1/4x</td>
				<td>490_SPS</td>
				<td>32_SPS</td>
			</tr>
            <tr>
				<td>1/2x</td>
				<td>920_SPS</td>
				<td>64_SPS</td>
			</tr>
            <tr>
				<td>1x (default)</td>
				<td>1600_SPS</td>
				<td>128_SPS</td>
			</tr>
            <tr>
				<td>2x</td>
				<td>2400_SPS</td>
				<td>250_SPS</td>
			</tr>
            <tr>
				<td>4x</td>
				<td>3300_SPS</td>
				<td>475_SPS</td>
			</tr>
            <tr>
				<td>8x</td>
				<td>3300_SPS</td>
				<td>860_SPS</td>
			</tr>
      </table>
			
			</ul>
		</li>

		<br><br>
	</ul>	
	The following entries are only valid in comparator mode and with thresholds which are currently disabled or not implemented, since my use case is a plain 4-channel A/D conversion.<br>
	Please refer to ADS1x1x chip documentation for more details on the effect of these settings.<br>
	<br>
	<ul>
		<li>comparator_mode(Traditional|Window)<br>
			<ul>
			Not implemented.
			</ul>
		</li>
		<br>
		<li>comparator_polarity (ActiveHigh|ActiveLow)<br>
			<ul>
			Not implemented.
			</ul>
		</li>
		<br>
		<li>comparator_queue_disable (AfterOneConversion|AfterTwoConversions|AfterFourConversions|disable)<br>
			<ul>
			Define for how many conversions the chip remains active (powered on)<br>
			</ul>
		</li>
		<br>		
		<li>latching_comparator (on|off)<br>
			<ul>
			Not implemented.
			</ul>
		</li>
		<br>			
		<br>			
		
		
		<li><a href="#IODev">IODev</a></li>
		<li><a href="#ignore">ignore</a></li>
		<li><a href="#do_not_notify">do_not_notify</a></li>
		<li><a href="#showtime">showtime</a></li>
	</ul>
	<br>
</ul>

=end html

=begin html_DE

<a name="I2C_ADS1x1x"></a>
<h3>I2C_ADS1x1x</h3>
(<a href="commandref.html#I2C_ADS1x1x">en</a> | de)
<ul>
	<a name="I2C_ADS1x1x"></a>
		Bitte englische Dokumentation verwenden.</b><br>
	<a name="I2C_ADS1x1xDefine"></a><br>
	<b>Define</b>
	<ul>
		<code>define &lt;name&gt; I2C_ADS1x1x &lt;I2C Address&gt;</code><br>
		Der Wert <code>&lt;I2C Address&gt;</code> ist ohne das Richtungsbit<br>
	</ul>

	<a name="I2C_ADS1x1xSet"></a>
	<b>Set</b>
	<ul>
	</ul>

	<a name="I2C_ADS1x1xAttr"></a>
	<b>Attribute</b>
	<ul>
		<li>poll_interval<br>
			Aktualisierungsintervall aller Werte in Minuten.<br>
			Standard: -, g&uuml;ltige Werte: Dezimalzahl<br><br>
		</li>
		<li><a href="#IODev">IODev</a></li>
		<li><a href="#ignore">ignore</a></li>
		<li><a href="#do_not_notify">do_not_notify</a></li>
		<li><a href="#showtime">showtime</a></li>
	</ul>
	<br>
</ul>

=end html_DE

=cut

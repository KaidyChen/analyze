#! /usr/bin/perl -w
use strict;
#use utf8;
use Config::IniFiles;
use Date::Parse;
use Date::Calc qw(Add_Delta_Days);
use File::Path;




######################配置config.ini####################
#[FILE]
#ZJJLog=F:\project\AirConditioner_of_WuHanUniversity\通讯质量分析\中间件日志\logorder2016_8_*.txt
#[DATA]
#timeout=31
my $cfg = new Config::IniFiles(-file => "config.ini");
my $ZJJLog_dir     = $cfg->val('FILE','ZJJLog_dir');
my $timeout        = $cfg->val('DATA','timeout');          #设置中间件超时时间
my $Commu_save_dir = $cfg->val('FILE','Commu_save_dir');




#获取当前系统时间
my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
$sec = ($sec < 10)? "0$sec":$sec;
$min = ($min < 10)? "0$min":$min;
$hour = ($hour < 10)? "0$hour":$hour;
$mday = ($mday < 10)? "0$mday":$mday;
$mon += 1;
$mon = ($mon < 10)? "0$mon":$mon;
$year += 1900;
my $YM_local = $year."-".$mon;    #当月

#根据当前日期获取上一日日期
my $calculate_days = $cfg->val('DATE','calculate_days');
my ($year_of_calculateDay,$month_of_calculateDay,$calculateDay) = Add_Delta_Days($year,$mon,$mday,$calculate_days);
#需要计算的中间件日志txt文件
my $logorder_txt = "logorder".$year_of_calculateDay."_".$month_of_calculateDay."_*.txt";
my $ZJJLog_path = $ZJJLog_dir."/".$logorder_txt;
my @filelist = glob $ZJJLog_path;

if ($month_of_calculateDay < 10) {
	$month_of_calculateDay = "0".$month_of_calculateDay;
}
if ($calculateDay < 10) {
	$calculateDay = "0".$calculateDay;
}
my $YM_calculateDay   = $year_of_calculateDay."-".$month_of_calculateDay;
my $date_calculateDay = $year_of_calculateDay."-".$month_of_calculateDay."-".$calculateDay;



my %hash;
my %count_hash;
my (%MeterErrSumcount,%MeterErrTypeCount,%MeterCommuniCount);
#发送命令的控制码有11、14、13、0f、1c、03、04、1a；
#正常返回的控制码有91、94、93、8f、9c、83、84、9a；
#异常返回的控制码有d1、d4、cf、dc、c3、c4、da，也包括无数据返回的情况
foreach my $file (@filelist) {
	open(MYFILE,"<",$file) or die("Can't open $file");
	while (my $log = <MYFILE>) {
		chomp $log;
		if ($log =~ /(\d{4}-\d+-\d+\s\d+:\d+:\d+):\d+.*\s(\w{12}).*(?:fa0101)?68(\d{12})68(\w{2})(\w{2})(\w+)?(\w{2})16/i) {
			       # 2016-8-18 0:5:17:99====>> 42435c5573fe：6878120027011668140c3434333735dff1ffab8967456e16
			my $datetime = $1;
			my $collector = $2;
			my $meter = $3;
			my $control = $4;
			my $len = $5;
			my $data = $6;
			my $cs = $7;

			if ((!exists $hash{$collector}{$meter}) && ($control eq "11" || $control eq "13" || $control eq "14" || $control eq "0F" || $control eq "03" || $control eq "04" || $control eq "1C" || $control eq "1A")) {
				$count_hash{$collector}{$meter} = 0;
				$hash{$collector}{$meter}{$count_hash{$collector}{$meter}}{"sendcontrol"} = $control;
				$hash{$collector}{$meter}{$count_hash{$collector}{$meter}}{"senddatetime"} = $datetime;
			}
			#排除连续收到2条返回的情况!(exists $hash{$meter}{$count_hash{$meter}+1})
			elsif ((exists $hash{$collector}{$meter}) && (!exists $hash{$collector}{$meter}{$count_hash{$collector}{$meter}+1}) && ($control eq "11" || $control eq "13" || $control eq "14" || $control eq "0F" || $control eq "03" || $control eq "04" || $control eq "1C" || $control eq "1A")) {
				$count_hash{$collector}{$meter}++;
				$hash{$collector}{$meter}{$count_hash{$collector}{$meter}}{"sendcontrol"} = $control;
				$hash{$collector}{$meter}{$count_hash{$collector}{$meter}}{"senddatetime"} = $datetime;
			}
			#收到的命令
			elsif ((exists $hash{$collector}{$meter}{$count_hash{$collector}{$meter}}{"sendcontrol"}) && ($control ne "11" || $control eq "13" || $control ne "14" || $control ne "0F" || $control ne "03" || $control ne "04" || $control ne "1C" || $control eq "1A")) {
				$hash{$collector}{$meter}{$count_hash{$collector}{$meter}}{"receivecontrol"} = $control;
				$hash{$collector}{$meter}{$count_hash{$collector}{$meter}}{"receivedatetime"} = $datetime;

				if ($control eq "df" && (substr($data,-2,2) eq "34")) {
					$hash{$collector}{$meter}{$count_hash{$collector}{$meter}}{"receivecontrol"} = "DF01";
				}
				elsif ($control eq "df" && (substr($data,-2,2) eq "35")) {
					$hash{$collector}{$meter}{$count_hash{$collector}{$meter}}{"receivecontrol"} = "DF02";
				}
				elsif ($control eq "df" && (substr($data,-2,2) eq "36")) {
					$hash{$collector}{$meter}{$count_hash{$collector}{$meter}}{"receivecontrol"} = "DF03";
				} 
				elsif ($control eq "df" && (substr($data,-2,2) eq "37")) {
					$hash{$collector}{$meter}{$count_hash{$collector}{$meter}}{"receivecontrol"} = "DF04";
				}
				elsif ($control eq "df" && (substr($data,-2,2) eq "38")) {
					$hash{$collector}{$meter}{$count_hash{$collector}{$meter}}{"receivecontrol"} = "DF05";
				}
				elsif ($control eq "df" && (substr($data,-2,2) eq "39")) {
					$hash{$collector}{$meter}{$count_hash{$collector}{$meter}}{"receivecontrol"} = "DF06";
				}

				elsif ($control eq "dc") {
					my $error_cha = hex(substr($data,-2,2))-51;
					$error_cha = sprintf("%x",$error_cha);
					if ($error_cha < 10) {
						$error_cha = "0".$error_cha;
					}
					$hash{$collector}{$meter}{$count_hash{$collector}{$meter}}{"receivecontrol"} = "DC:".$error_cha;
				}
				elsif ($control eq "d1") {
					my $error_cha = hex(substr($data,-2,2))-51;
					$error_cha = sprintf("%x",$error_cha);
					if ($error_cha < 10) {
						$error_cha = "0".$error_cha;
					}
					$hash{$collector}{$meter}{$count_hash{$collector}{$meter}}{"receivecontrol"} = "D1:".$error_cha;
				}
				elsif ($control eq "c3") {
					my $error_cha = hex(substr($data,-2,2))-51;
					$error_cha = sprintf("%x",$error_cha);
					if ($error_cha < 10) {
						$error_cha = "0".$error_cha;
					}
					$hash{$collector}{$meter}{$count_hash{$collector}{$meter}}{"receivecontrol"} = "C3:".$error_cha;
				}
			}
		}
	}    
	close MYFILE;
}




my %rate;    #通讯成功率
foreach my $collector (keys %hash) {   
	foreach my $meter (keys %{$hash{$collector}}) {
		foreach my $n (sort keys %{$hash{$collector}{$meter}}) {
			my $a;
			if (defined $n && $n ne "") {
				if (exists $hash{$collector}{$meter}{$n}{"receivecontrol"}) {
					#计算命令响应时间
					my $response_time = timestamp($hash{$collector}{$meter}{$n}{"senddatetime"},$hash{$collector}{$meter}{$n}{"receivedatetime"});
					if ($response_time >= $timeout) {
						$hash{$collector}{$meter}{$n}{"receivecontrol"} = "timeout";
					}
				}
				if ((exists $hash{$collector}{$meter}{$n}{"sendcontrol"}) && !(exists $hash{$collector}{$meter}{$n}{"receivecontrol"})) {
					$hash{$collector}{$meter}{$n}{"receivecontrol"} = "no response";
					$hash{$collector}{$meter}{$n}{"receivedatetime"} = $hash{$collector}{$meter}{$n}{"senddatetime"};
				}
				#若不是正常返回的控制码，则表示为通讯失败
				if ((defined $hash{$collector}{$meter}{$n}{"receivecontrol"}) && $hash{$collector}{$meter}{$n}{"receivecontrol"} ne "91" && $hash{$collector}{$meter}{$n}{"receivecontrol"} ne "94" && $hash{$collector}{$meter}{$n}{"receivecontrol"} ne "8f" && $hash{$collector}{$meter}{$n}{"receivecontrol"} ne "b1" && $hash{$collector}{$meter}{$n}{"receivecontrol"} ne "83" && $hash{$collector}{$meter}{$n}{"receivecontrol"} ne "84" && $hash{$collector}{$meter}{$n}{"receivecontrol"} ne "9c" && $hash{$collector}{$meter}{$n}{"receivecontrol"} ne "b2" && $hash{$collector}{$meter}{$n}{"receivecontrol"} ne "9a") {
					$a = $hash{$collector}{$meter}{$n}{"receivecontrol"};
					$MeterErrSumcount{$collector}{$meter}++;         #电表通讯失败次数		
				}
				$MeterCommuniCount{$collector}{$meter}++;            #电表通讯次数
			}
			
		}
		#表通讯失败概率统计
		my $MeterErrRate     = 0;
		my $MeterSuccessRate = 0;
		if ((exists $MeterErrSumcount{$collector}{$meter}) && ($MeterCommuniCount{$collector}{$meter}>0)) {
			$MeterErrRate = $MeterErrSumcount{$collector}{$meter}/$MeterCommuniCount{$collector}{$meter};
			$MeterErrRate = sprintf("%.2f",$MeterErrRate);
			$MeterSuccessRate = 1 - $MeterErrRate;
		}
		#将通讯成功率数据写入txt文件
		if (exists $MeterCommuniCount{$collector}{$meter}) {
			$meter =~ s/(\d{2})(\d{2})(\d{2})(\d{2})(\d{2})(\d{2})/$6$5$4$3$2$1/;
			my $temp_dir = $Commu_save_dir."/".$meter."/used_commu_of_month";
			$meter =~ s/(\d{2})(\d{2})(\d{2})(\d{2})(\d{2})(\d{2})/$6$5$4$3$2$1/;
			if (!-e $temp_dir) {
				mkpath($temp_dir);
			}
			my $outfile = $temp_dir."/".$year_of_calculateDay.".txt";
			if (!-e $outfile) {
				open(TMP,">",$outfile) or die("can't creat $outfile");
				close TMP;
			}
			open(TEMP,"<",$outfile) or die("can't open $outfile");
			while (my $data = <TEMP>) {
				chomp $data;
				my $year_month   = (split(" ",$data))[0];
				my $success_rate = (split(" ",$data))[1];
				$rate{$collector}{$meter}{$year_month} = $success_rate;
			}
			$rate{$collector}{$meter}{$YM_calculateDay} = $MeterSuccessRate;
			close TEMP;
			open(TEMP2,">",$outfile) or die("can't open $outfile");
			foreach my $year_month (sort {$a cmp $b} keys %{$rate{$collector}{$meter}}) {
				print TEMP2 $year_month," ",$rate{$collector}{$meter}{$year_month},"\n";
			}
			close TEMP2;
		}
		
	}
}








#时间戳函数（秒）
sub timestamp{
    my ($dt1,$dt2) = @_;
	my $t1 = str2time($dt1);
	my $t2 = str2time($dt2);
	my $t = $t2 - $t1;
	return $t;
}

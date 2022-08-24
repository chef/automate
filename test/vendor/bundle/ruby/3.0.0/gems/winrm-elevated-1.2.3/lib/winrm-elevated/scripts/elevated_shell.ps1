$username = '<%= username %>'
$password = '<%= password %>'
$script_file = '<%= script_path %>'

$interactive = '<%= interactive_logon %>'
$pass_to_use = $password
$logon_type = 1
$logon_type_xml = "<LogonType>Password</LogonType>"
if($pass_to_use.length -eq 0) {
  $pass_to_use = $null
  $logon_type = 5
  $logon_type_xml = ""
}
if($interactive -eq 'true') {
  $logon_type = 3
  $logon_type_xml = "<LogonType>InteractiveTokenOrPassword</LogonType>"
}

$task_name = "WinRM_Elevated_Shell_" + [guid]::NewGuid()
$out_file = [System.IO.Path]::GetTempFileName()
$err_file = [System.IO.Path]::GetTempFileName()

$task_xml = @'
<?xml version="1.0" encoding="UTF-16"?>
<Task version="1.2" xmlns="http://schemas.microsoft.com/windows/2004/02/mit/task">
  <Principals>
    <Principal id="Author">
      <UserId>{username}</UserId>
      {logon_type}
      <RunLevel>HighestAvailable</RunLevel>
    </Principal>
  </Principals>
  <Settings>
    <MultipleInstancesPolicy>IgnoreNew</MultipleInstancesPolicy>
    <DisallowStartIfOnBatteries>false</DisallowStartIfOnBatteries>
    <StopIfGoingOnBatteries>false</StopIfGoingOnBatteries>
    <AllowHardTerminate>true</AllowHardTerminate>
    <StartWhenAvailable>false</StartWhenAvailable>
    <RunOnlyIfNetworkAvailable>false</RunOnlyIfNetworkAvailable>
    <IdleSettings>
      <StopOnIdleEnd>false</StopOnIdleEnd>
      <RestartOnIdle>false</RestartOnIdle>
    </IdleSettings>
    <AllowStartOnDemand>true</AllowStartOnDemand>
    <Enabled>true</Enabled>
    <Hidden>false</Hidden>
    <RunOnlyIfIdle>false</RunOnlyIfIdle>
    <WakeToRun>false</WakeToRun>
    <ExecutionTimeLimit>PT24H</ExecutionTimeLimit>
    <Priority>4</Priority>
  </Settings>
  <Actions Context="Author">
    <Exec>
      <Command>cmd</Command>
      <Arguments>{arguments}</Arguments>
    </Exec>
  </Actions>
</Task>
'@

$arguments = "/c powershell.exe -executionpolicy bypass -NoProfile -File $script_file &gt; $out_file 2&gt;$err_file"

$task_xml = $task_xml.Replace("{arguments}", $arguments)
$task_xml = $task_xml.Replace("{username}", $username)
$task_xml = $task_xml.Replace("{logon_type}", $logon_type_xml)

try {
    $schedule = New-Object -ComObject "Schedule.Service"
    $schedule.Connect()
    $task = $schedule.NewTask($null)
    $task.XmlText = $task_xml
    $folder = $schedule.GetFolder("\")
    $folder.RegisterTaskDefinition($task_name, $task, 6, $username, $pass_to_use, $logon_type, $null) | Out-Null

    $registered_task = $folder.GetTask("\$task_name")
    $registered_task.Run($null) | Out-Null

    $timeout = 10
    $sec = 0
    while ( (!($registered_task.state -eq 4)) -and ($sec -lt $timeout) ) {
        Start-Sleep -s 1
        $sec++
    }
} catch {
    Write-Error -ErrorRecord $PSItem
    exit $PSItem.Exception.HResult
}

function SlurpOutput($file, $cur_line, $out_type) {
  if (Test-Path $file) {
    $fs = New-Object -TypeName System.IO.FileStream -ArgumentList @(
      $file,
      [system.io.filemode]::Open,
      [System.io.FileAccess]::Read,
      [System.IO.FileShare]::ReadWrite
    )
    try {
      $enc = [System.Text.Encoding]::GetEncoding($Host.CurrentCulture.TextInfo.OEMCodePage)
      $bytes = [System.Byte[]]::CreateInstance([System.Byte], $fs.Length)
      if ($fs.Read($bytes, 0, $fs.Length) -gt 0) {
        $text = $enc.GetString($bytes)
        $text.TrimEnd("`n").TrimEnd("`r").Split(@("`r`n", "`n"), [StringSplitOptions]::None) | Select-Object -skip $cur_line | ForEach-Object {
          $cur_line += 1
          if ($out_type -eq 'err') {
            $host.ui.WriteErrorLine("$_")
          } else {
            $host.ui.WriteLine("$_")
          }
        }
      }
    }
    finally { $fs.Close() }
  }
  return $cur_line
}

$err_cur_line = 0
$out_cur_line = 0
$timeout = <%= execution_timeout %>
$startDate = Get-Date
do {
  Start-Sleep -m 100
  $out_cur_line = SlurpOutput $out_file $out_cur_line 'out'
  $err_cur_line = SlurpOutput $err_file $err_cur_line 'err'
} while( (!($registered_task.state -eq 3)) -and ($startDate.AddSeconds($timeout) -gt (Get-Date)) )

# We'll make a best effort to clean these files
# But a reboot could possibly end the task while the process
# still runs and locks the file. If we can't delete we don't want to fail
try { Remove-Item $out_file -ErrorAction Stop } catch {}
try { Remove-Item $err_file -ErrorAction Stop } catch {}
try { Remove-Item $script_file -ErrorAction Stop } catch {}

$exit_code = $registered_task.LastTaskResult

try {
  # Clean current task
  $folder.DeleteTask($task_name, 0)
  # Clean old tasks if required
  $old_tasks_filter_date = [datetime]::Now.AddSeconds(<%= -1 * execution_timeout %>)
  $old_tasks_to_kill = $folder.GetTasks(0) | Select Name,LastRunTime | Where-Object {
    ($_.Name -like "WinRM_Elevated_Shell*") -and ($_.LastRunTime -le $old_tasks_filter_date) -and ($_.Name -ne $task_name)
  }
  $old_tasks_to_kill | ForEach-Object { try { $folder.DeleteTask($_.Name, 0) } catch {} }
}
catch {}

[System.Runtime.Interopservices.Marshal]::ReleaseComObject($schedule) | Out-Null

exit $exit_code

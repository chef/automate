+++
title = "Automate HA Calculator"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Automate HA Calculator"
    parent = "automate/deploy_high_availability/ha_system_requirements"
    identifier = "automate/deploy_high_availability/ha_system_requirements/ha_hardware-calculator.md Automate HA Calculator"
    weight = 205
+++

<!DOCTYPE html>
<html>
<body>

	<form onsubmit="return calculation();">
  		Number of Nodes Sending Data = <input name="Number of Nodes Sending Data" size="8" id="nodes_sending_data"></br></br></br>
  		Frequency of Compliance Scan = <input name="Frequency of Compliance Scan" size="8" id="frequency_of_compliance"></br></br></br>
        Frequency of Client runs = <input name="Frequency of Client runs" size= "8" id="frequency_of_clientruns"></br></br></br>
        Frequency of Event Feed = <input name="Frequency of Event Feed" size= "8" id="frequency_of_event_feed"></br></br></br>
        Data Retention Policy = <input name="Data Retention Policy" size= "8" id="data_retention_policy"></br></br></br>
        <input type="submit" value="Click here"/></br></br></br>
  		Output = <input type="number" id="varResult" readonly/>
	</form>
    <script>
function calculation () {
  var a = parseInt(document.getElementById("nodes_sending_data").value);
  var b = parseInt(document.getElementById("frequency_of_compliance").value);
  var c = parseInt(document.getElementById("frequency_of_clientruns").value);
  var d = parseInt(document.getElementById("frequency_of_event_feed").value);
  var e = parseInt(document.getElementById("data_retention_policy").value);

  var result = [(a * b) + (c * d)] - e;

  document.getElementById("varResult").value = result;
  return false;
}

</script>

</body>
</html>

output "habitat_info" {
  value = "Habitat IDs: ${join(",", null_resource.habitat.*.id)}\nHabitat Info: ${local.habitat_info}"
}

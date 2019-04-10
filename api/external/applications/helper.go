package applications

func (m *HabService) FullServiceGroupName() string {
	return m.GetPkgIdent().GetName() + "." + m.GetGroup()
}

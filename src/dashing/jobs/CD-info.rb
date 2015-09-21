VCS_ROOT = RADIATOR_CONFIG[:vcs_root]
SCHEDULER.every '15m', first_in: 0 do
  output = `jobs/CD-info-get.sh '#{VCS_ROOT}'`
  val = output.match(/.* (.*)/)[1]

  if val == "NA"
    val = "-"
  end

  send_event('cdinfoupdate',   { oldestDoneFeature: val })
end

#
# Regular cron jobs for the roswell package
#
0 4	* * *	root	[ -x /usr/bin/roswell_maintenance ] && /usr/bin/roswell_maintenance

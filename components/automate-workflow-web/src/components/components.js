import ng from 'angular';
import ace from './ace/ace';
import changeButtons from './change_buttons/change_buttons';
import changeCard from './change_card/change_card';
import comment from './comment/comment';
import commentForm from './comment_form/comment_form';
import diff from './diff/diff';
import diffStats from './diff_stats/diff_stats';
import embedly from './embedly/embedly';
import fileComments from './file_comments/file_comments';
import focusIf from './focus_if/focus_if';
import groupedPhaseList from './grouped_phase_list/grouped_phase_list';
import header from './header/header';
import icon from './icon/icon';
import iconButton from './icon_button/icon_button';
import infiniteScroll from './infinite_scroll/infinite_scroll';
import info from './info/info';
import lineComments from './line_comments/line_comments';
import log from './log/log';
import logCli from './log/log_cli';
import logCompliance from './log/log_compliance';
import markdown from './markdown/markdown';
import menu from './menu/menu';
import navbar from './navbar/navbar';
import organizations from './organizations/organizations';
import pageDrawer from './page_drawer/page_drawer';
import patchsetCard from './patchset_card/patchset_card';
import patchsetStatus from './patchset_status/patchset_status';
import projectCard from './project_card/project_card';
import projectRepo from './project_repo/project_repo';
import quickFind from './quick_find/quick_find';
import relativeTime from './relative_time/relative_time';
import rolesField from './roles_field/roles_field';
import runDetailItem from './grouped_phase_list/run_detail_item/run_detail_item';
import samlSetup from './saml_setup/saml_setup';
import scmTypes from './scm_types/scm_types';
import searches from './searches/searches';
import select from './select/select';
import smtpSetup from './smtp_setup/smtp_setup';
import statusIcon from './status_icon/status_icon';
import tabset from './tabset/tabset';
import textExpander from './text_expander/text_expander';
import teams from './teams/teams';
import team from './teams/team';
import userDropdown from './user_dropdown/user_dropdown';
import users from './users/users';

export default ng
  .module('cd.components', [
    ace,
    changeButtons,
    changeCard,
    comment,
    commentForm,
    diff,
    diffStats,
    embedly,
    fileComments,
    groupedPhaseList,
    header,
    icon,
    iconButton,
    infiniteScroll,
    info,
    lineComments,
    log,
    logCli,
    logCompliance,
    markdown,
    menu,
    navbar,
    organizations,
    pageDrawer,
    patchsetCard,
    patchsetStatus,
    projectCard,
    projectRepo,
    quickFind,
    relativeTime,
    rolesField,
    runDetailItem,
    samlSetup,
    scmTypes,
    searches,
    select,
    smtpSetup,
    statusIcon,
    tabset,
    teams,
    team,
    textExpander,
    userDropdown,
    users
  ])
  .name;

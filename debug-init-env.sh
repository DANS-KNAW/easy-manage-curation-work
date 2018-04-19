#!/usr/bin/env bash
#
# Copyright (C) 2017 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

DATADIR=data
EASY_COMMON_CURATION_AREA=$DATADIR/easy-common-curation-area
DATAMANAGER_CURATION_AREAS=$DATADIR/datamanager-curation-areas

echo "Pre-creating log..."
touch $DATADIR/easy-manage-curation-work.log

echo "Delete test data from $EASY_COMMON_CURATION_AREA..."
rm -r ${EASY_COMMON_CURATION_AREA}
echo "Delete test data from $DATAMANAGER_CURATION_AREAS..."
rm -r ${DATAMANAGER_CURATION_AREAS}

echo "Copy test input into $EASY_COMMON_CURATION_AREA.."
cp -r src/test/resources/easy-common-curation-area ${DATADIR}
echo "Copy test input into $DATAMANAGER_CURATION_AREAS.."
cp -r src/test/resources/datamanager-curation-areas ${DATAMANAGER_CURATION_AREAS}

echo "OK"
